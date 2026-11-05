execution_abort <- function(message, class = "xnatR_verification_error") {
  rlang::abort(message, class = class)
}
execution_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' Execute a frozen DICOM plan with verified restart
#'
#' Downloads ZIP archives without extracting them. Rechecks source catalogs,
#' validates relative archive paths, sizes and CRCs, then publishes SHA-256
#' receipts. Reruns reuse only archives matching their receipt and current source
#' catalog. Incomplete or invalid artifacts are retained, not overwritten.
#'
#' @param plan A validated xnat_download_plan or a saved JSON plan path.
#' @param client Client for the plan's server.
#' @param download_timeout Maximum seconds per archive transfer.
#' @param verification_timeout Maximum seconds for each system unzip CRC test.
#' @param continue_on_error Continue to later resources after a resource failure.
#'   Defaults to FALSE. Source changes always stop the run.
#' @param progress Display download progress when supported by the backend.
#' @param page_size,pagination,max_pages Collection traversal controls as in
#'   [xnat_inventory()].
#' @return An xnat_download_result with explicit complete/state fields, one row
#'   per planned resource, retained scan exclusions, and a summary_path. Errors
#'   during execution and user interruption yield an incomplete result. Validation
#'   and lock acquisition can throw before execution begins.
#'   Requires system unzip and a filesystem supporting advisory locks.
#' @export
xnat_execute_plan <- function(plan, client = NULL, download_timeout = 3600,
                              verification_timeout = 3600, continue_on_error = FALSE,
                              progress = TRUE, page_size = 100L,
                              pagination = c("auto", "paged", "all"), max_pages = 10000L) {
  if (is.character(plan)) plan <- xnat_read_plan(plan)
  validate_download_plan(plan)
  client <- client %||% xnat_current_client()
  require_auth(client)
  if (!identical(client$base_url, plan$server_url)) execution_abort("Plan and client refer to different servers.")
  for (value in list(download_timeout, verification_timeout)) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) execution_abort("Timeouts must be positive finite seconds.")
  }
  if (!is.logical(continue_on_error) || length(continue_on_error) != 1L || is.na(continue_on_error)) execution_abort("continue_on_error must be TRUE or FALSE.")
  pagination <- match.arg(pagination)
  unzip <- Sys.which("unzip")
  if (!nzchar(unzip)) execution_abort("System unzip is required for CRC verification.")
  root <- plan$destination_root
  if (!dir.exists(root) && !dir.create(root, recursive = TRUE, mode = "0700")) execution_abort("Cannot create destination root.")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  lock_path <- file.path(root, ".xnatr-executor.lock")
  execution_safe_path(lock_path, root)
  # Keep the lock file: removing it can give competing processes different inodes.
  lock <- filelock::lock(lock_path, timeout = 0)
  if (is.null(lock)) execution_abort("Destination is locked by another executor.", "xnatR_lock_error")
  on.exit(filelock::unlock(lock), add = TRUE)
  Sys.chmod(lock_path, "0600")
  fingerprint <- plan_hash(unclass(plan))
  summary_path <- file.path(root, paste0("run-summary-", fingerprint, ".json"))
  execution_safe_path(summary_path, root)
  rows <- plan$resources[, c("resource_key", "experiment_id", "scan_id", "expected_files", "expected_bytes", "destination")]
  rows$status <- "pending"
  rows$error_class <- NA_character_
  rows$partial_path <- NA_character_
  rows$receipt_path <- paste0(rows$destination, ".receipt.json")
  result <- structure(list(schema_version = 1L, plan_sha256 = fingerprint,
    server_url = plan$server_url, started_at = execution_time(), finished_at = NULL,
    state = "running", complete = FALSE, source_verified = FALSE,
    expected_resources = plan$expected_resources, verified_resources = 0L,
    expected_files = plan$expected_files, verified_files = 0L,
    expected_bytes = plan$expected_bytes, resources = rows,
    exclusions = plan$scans[plan$scans$disposition == "no_dicom", , drop = FALSE],
    error_class = NA_character_, summary_path = summary_path), class = "xnat_download_result")
  save <- function() execution_json(unclass(result), summary_path, root)
  save() # Invalidates an earlier completion marker before any source requests.
  collect <- function(path, key = "ID") xnat_collect(path, key, client, page_size, pagination, max_pages)
  active <- NA_integer_
  tryCatch({
    execution_selection(plan, collect)
    for (i in seq_len(nrow(plan$resources))) execution_catalog(plan, i, collect)
    for (i in seq_len(nrow(plan$resources))) {
      active <- i
      resource <- plan$resources[i, , drop = FALSE]
      dest <- file.path(root, paste0(resource$resource_key, ".zip"))
      receipt_path <- paste0(dest, ".receipt.json")
      execution_mkdir(dirname(dest), root)
      execution_safe_path(dest, root)
      execution_safe_path(receipt_path, root)
      tryCatch({
        execution_catalog(plan, i, collect)
        catalog <- plan$files[plan$files$resource_key == resource$resource_key, c("name", "uri", "bytes")]
        if (execution_reusable(dest, receipt_path, fingerprint, resource, plan$server_url)) {
          execution_verify_zip(dest, catalog, unzip, verification_timeout)
          result$resources$status[i] <- "reused"
        } else {
          execution_preserve(dest, root)
          execution_preserve(receipt_path, root)
          partial <- tempfile(paste0(substr(basename(dest), 1L, 160L), ".partial-"), tmpdir = dirname(dest))
          result$resources$partial_path[i] <- partial
          result$resources$status[i] <- "downloading"
          save()
          if (!file.create(partial, showWarnings = FALSE)) execution_abort("Cannot create partial archive.")
          Sys.chmod(partial, "0600")
          execution_download(resource$path, partial, client, download_timeout, progress)
          execution_verify_zip(partial, catalog, unzip, verification_timeout)
          execution_catalog(plan, i, collect) # Detect changes during the transfer.
          receipt <- list(schema_version = 1L, plan_sha256 = fingerprint,
            server_url = plan$server_url, resource_key = resource$resource_key,
            catalog_sha256 = resource$catalog_sha256,
            archive_sha256 = execution_hash(partial), archive_bytes = execution_size(partial),
            expected_files = resource$expected_files, expected_bytes = resource$expected_bytes,
            verification = "relative-paths-sizes-unzip-crc-v1", verified_at = execution_time())
          execution_safe_path(dest, root)
          if (file.exists(dest) || !file.rename(partial, dest)) execution_abort("Cannot publish verified archive.")
          execution_json(receipt, receipt_path, root)
          result$resources$status[i] <- "downloaded"
        }
      }, error = function(e) {
        result$resources$status[i] <<- if (inherits(e, "xnatR_source_changed")) "source_changed" else "failed"
        result$resources$error_class[i] <<- class(e)[1]
      })
      save()
      if (result$resources$status[i] == "source_changed" ||
          (result$resources$status[i] == "failed" && !continue_on_error)) break
      active <- NA_integer_
    }
    if (all(result$resources$status %in% c("downloaded", "reused"))) {
      execution_selection(plan, collect)
      for (i in seq_len(nrow(plan$resources))) {
        active <- i
        execution_catalog(plan, i, collect)
        resource <- plan$resources[i, , drop = FALSE]
        dest <- file.path(root, paste0(resource$resource_key, ".zip"))
        if (!execution_reusable(dest, paste0(dest, ".receipt.json"), fingerprint, resource, plan$server_url)) execution_abort("Published archive or receipt changed during the run.")
      }
      result$source_verified <- TRUE
      result$complete <- TRUE
    }
    result$state <- if (result$complete) "complete" else "incomplete"
  }, interrupt = function(e) {
    result$state <<- "interrupted"
    if (!is.na(active)) result$resources$status[active] <<- "interrupted"
  }, error = function(e) {
    result$state <<- "incomplete"
    result$error_class <<- class(e)[1]
    if (!is.na(active)) {
      result$resources$status[active] <<- if (inherits(e, "xnatR_source_changed")) "source_changed" else "failed"
      result$resources$error_class[active] <<- class(e)[1]
    }
  })
  verified <- result$resources$status %in% c("downloaded", "reused")
  result$verified_resources <- sum(verified)
  result$verified_files <- sum(result$resources$expected_files[verified])
  result$finished_at <- execution_time()
  save()
  result
}

execution_download <- function(path, dest, client, timeout, progress) {
  req <- xnat_request(path, client) |>
    httr2::req_url_query(format = "zip") |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 1)
  if (isTRUE(progress)) req <- httr2::req_progress(req)
  xnat_perform(req, client, path = dest)
  invisible(dest)
}

execution_selection <- function(plan, collect) {
  for (i in seq_len(nrow(plan$sessions))) {
    session <- plan$sessions[i, ]
    members <- collect(xnat_path("data/projects", url_encode(session$project_id), "subjects",
      url_encode(session$subject_id), "experiments"))
    if (!session$experiment_id %in% inventory_text(members, "ID")) execution_abort("Session membership changed.", "xnatR_source_changed")
  }
  for (i in seq_len(nrow(plan$scans))) {
    scan <- plan$scans[i, ]
    current <- collect(xnat_path("data/experiments", url_encode(scan$experiment_id), "scans",
      url_encode(scan$scan_id), "resources"), "xnat_abstractresource_id")
    ids <- inventory_text(current, "xnat_abstractresource_id")
    labels <- inventory_text(current, "label")
    ids <- ids[!is.na(labels) & labels == "DICOM"]
    expected <- plan$resources$resource_id[plan$resources$experiment_id == scan$experiment_id & plan$resources$scan_id == scan$scan_id]
    if (!setequal(ids, expected)) execution_abort("DICOM resource selection changed.", "xnatR_source_changed")
  }
}
execution_catalog <- function(plan, i, collect) {
  resource <- plan$resources[i, ]
  current <- collect(resource$path, "URI")
  catalog <- tibble::tibble(name = inventory_text(current, "Name"),
    uri = inventory_text(current, "URI"), bytes = decimal_values(inventory_text(current, "Size")))
  catalog <- catalog[order(catalog$uri), , drop = FALSE]
  if (anyNA(catalog) || !identical(plan_hash(catalog), resource$catalog_sha256)) execution_abort("Source catalog changed; create and review a new plan.", "xnatR_source_changed")
  invisible(TRUE)
}

execution_hash <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
execution_size <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size >= 2^53) execution_abort("Cannot represent archive size exactly.")
  format(size, scientific = FALSE, trim = TRUE, digits = 22)
}
execution_reusable <- function(dest, receipt_path, fingerprint, resource, server) {
  if (!file.exists(dest) || !file.exists(receipt_path)) return(FALSE)
  tryCatch({
    receipt <- jsonlite::fromJSON(receipt_path, simplifyVector = FALSE)
    identical(receipt$schema_version, 1L) && identical(receipt$plan_sha256, fingerprint) &&
      identical(receipt$server_url, server) && identical(receipt$resource_key, resource$resource_key) &&
      identical(receipt$catalog_sha256, resource$catalog_sha256) &&
      identical(receipt$verification, "relative-paths-sizes-unzip-crc-v1") &&
      isTRUE(receipt$expected_files == resource$expected_files) && identical(receipt$expected_bytes, resource$expected_bytes) &&
      identical(receipt$archive_bytes, execution_size(dest)) && identical(receipt$archive_sha256, execution_hash(dest))
  }, error = function(e) FALSE)
}

# No extraction: compare every ZIP member to a catalog-relative path and size,
# allowing only a single common XNAT wrapper directory prefix.
execution_verify_zip <- function(path, catalog, unzip, timeout) {
  entries <- tryCatch(zip::zip_list(path), error = function(e) execution_abort("Invalid ZIP directory."))
  names <- entries$filename
  safe <- function(x) {
    !is.na(x) & nzchar(x) & !grepl("^[A-Za-z]:|^/|\\\\|[\r\n]", x) &
      !vapply(strsplit(sub("/$", "", x), "/", fixed = TRUE), function(parts) any(parts %in% c(".", "..", "")), logical(1))
  }
  if (!all(safe(names)) || anyDuplicated(names)) execution_abort("Unsafe or duplicate ZIP member paths.")
  if ("type" %in% names(entries) && any(!entries$type %in% c("file", "directory"))) execution_abort("ZIP contains non-regular members.")
  if ("encryption" %in% names(entries) && any(is.na(entries$encryption) | entries$encryption != "none")) execution_abort("Encrypted ZIP members are unsupported.")
  entries <- entries[!endsWith(names, "/"), , drop = FALSE]
  if (any(!is.finite(entries$uncompressed_size) | entries$uncompressed_size >= 2^53)) execution_abort("ZIP member size exceeds exact verification range.")
  marker <- regexpr("/resources/[^/]+/files/", catalog$uri)
  if (any(marker < 0L)) execution_abort("Catalog URI cannot be mapped to a resource-relative file path.")
  relative <- utils::URLdecode(substring(catalog$uri, marker + attr(marker, "match.length")))
  if (!identical(basename(relative), basename(catalog$name))) execution_abort("Catalog names disagree with URI paths.")
  if (!all(safe(relative)) || anyDuplicated(relative)) execution_abort("Unsafe or ambiguous catalog-relative paths.")
  candidates <- entries$filename[endsWith(entries$filename, relative[1])]
  prefixes <- unique(substr(candidates, 1L, nchar(candidates) - nchar(relative[1])))
  prefixes <- prefixes[prefixes == "" | endsWith(prefixes, "/")]
  matched <- FALSE
  for (prefix in prefixes) {
    expected <- paste0(prefix, relative)
    idx <- match(expected, entries$filename)
    if (nrow(entries) == length(expected) && !anyNA(idx)) {
      sizes <- vapply(entries$uncompressed_size[idx], function(x) format(x, scientific = FALSE, trim = TRUE, digits = 22), character(1))
      if (identical(unname(sizes), unname(catalog$bytes))) matched <- TRUE
    }
  }
  if (!matched) execution_abort("ZIP members or sizes disagree with the catalog.")
  check <- tryCatch(processx::run(unzip, c("-tqq", normalizePath(path, mustWork = TRUE)),
    stdin = "/dev/null", timeout = timeout, error_on_status = FALSE,
    env = c("UNZIP" = "", "UNZIPOPT" = "", "ZIPINFO" = "", "ZIPINFOOPT" = "")),
    error = function(e) execution_abort("ZIP CRC verification could not complete."))
  if (check$status != 0L) execution_abort("ZIP CRC verification failed.")
  invisible(TRUE)
}

execution_is_link <- function(path) {
  link <- Sys.readlink(path)
  !is.na(link) && nzchar(link)
}
execution_safe_path <- function(path, root) {
  parent <- dirname(path)
  while (!identical(parent, root)) {
    if (identical(parent, dirname(parent))) execution_abort("Destination escapes its root.")
    if (execution_is_link(parent)) execution_abort("Symlinked destination directories are unsupported.")
    if (file.exists(parent) && !dir.exists(parent)) execution_abort("Destination parent is not a directory.")
    parent <- dirname(parent)
  }
  if (execution_is_link(path) || dir.exists(path) || (file.exists(path) && !utils::file_test("-f", path))) execution_abort("Destination is a symlink or directory.")
  invisible(TRUE)
}
execution_mkdir <- function(path, root) {
  execution_safe_path(file.path(path, ".check"), root)
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, mode = "0700")) execution_abort("Cannot create archive directory.")
}
execution_preserve <- function(path, root) {
  execution_safe_path(path, root)
  if (file.exists(path)) {
    retained <- tempfile(paste0(substr(basename(path), 1L, 160L), ".unverified-"), tmpdir = dirname(path))
    if (!file.rename(path, retained)) execution_abort("Cannot preserve unverified artifact.")
  }
}
execution_json <- function(value, path, root) {
  execution_safe_path(path, root)
  tmp <- tempfile(".xnatr-json-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  if (!file.create(tmp, showWarnings = FALSE)) execution_abort("Cannot create receipt or summary.")
  Sys.chmod(tmp, "0600")
  writeLines(jsonlite::prettify(plan_json(value)), tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) execution_abort("Cannot publish receipt or summary.")
  invisible(path)
}

#' @export
print.xnat_download_result <- function(x, ...) {
  cat(sprintf("XNAT download: %s; %d/%d resources, %d/%d files verified\n",
    x$state, x$verified_resources, x$expected_resources, x$verified_files, x$expected_files))
  print(x$resources, ...)
  invisible(x)
}

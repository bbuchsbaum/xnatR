#' Build a reviewable DICOM download plan
#'
#' Freezes an explicit selection from an inventory. Rechecks session membership,
#' scan/resource identities and selected session metadata before collecting every
#' DICOM file catalog. Unknown sizes or identities and changed sources are refused.
#' Scans without DICOM resources remain visible as `no_dicom` dispositions.
#'
#' @param inventory An object returned by [xnat_inventory()].
#' @param experiment_ids Exact canonical session IDs to select. Ambiguous IDs
#'   shared across inventory project contexts are refused; narrow the inventory.
#' @param dest_dir Root directory for planned archives; no directories or downloads
#'   are created. Destinations encode IDs as UTF-8 hex to avoid path collisions.
#' @param scans Optional data frame with experiment_id and scan_id columns for an
#'   exact scan selection. NULL includes every scan in the selected sessions.
#' @param client XNAT client for the same server as the inventory.
#' @return An `xnat_download_plan` with session and scan selections, canonical
#'   resource paths, file catalogs, exact decimal byte totals, destination paths,
#'   and SHA-256 catalog fingerprints. These fingerprints identify metadata;
#'   they are not checksums of downloaded or remote file bytes.
#' @export
xnat_plan_download <- function(inventory, experiment_ids, dest_dir, scans = NULL, client = NULL) {
  if (!inherits(inventory, "xnat_inventory")) inventory_abort("inventory must be an xnat_inventory.")
  inventory_selection(experiment_ids, "experiment_ids")
  client <- client %||% xnat_current_client()
  require_auth(client)
  if (!identical(client$base_url, inventory$server_url)) inventory_abort("Inventory and client must refer to the same server.")
  selected <- inventory$sessions[inventory$sessions$experiment_id %in% experiment_ids, , drop = FALSE]
  if (anyDuplicated(selected$experiment_id) || !setequal(selected$experiment_id, experiment_ids)) {
    inventory_abort("Selection contains missing or ambiguous canonical experiment IDs.")
  }
  selected <- selected[match(experiment_ids, selected$experiment_id), , drop = FALSE]
  if (anyNA(selected$subject_id) || any(selected$subject_status == "unknown")) inventory_abort("Selected sessions have unresolved subject identities.")
  root <- plan_root(dest_dir)
  selection <- inventory$scans[inventory$scans$experiment_id %in% experiment_ids &
    inventory$scans$project_id %in% selected$project_id, , drop = FALSE]
  if (!is.null(scans)) {
    if (!is.data.frame(scans) || !all(c("experiment_id", "scan_id") %in% names(scans)) || !nrow(scans)) inventory_abort("scans must be a non-empty experiment_id/scan_id table.")
    wanted <- plan_keys(scans, c("experiment_id", "scan_id"))
    known <- plan_keys(selection, c("experiment_id", "scan_id"))
    if (anyDuplicated(wanted) || any(!wanted %in% known)) inventory_abort("Scan selection contains duplicate or unknown canonical IDs.")
    selection <- selection[match(wanted, known), , drop = FALSE]
  }
  if (!setequal(selection$experiment_id, experiment_ids)) inventory_abort("Every selected session must have at least one selected scan.")
  selection$disposition <- "no_dicom"
  collect <- function(path, key = "ID") xnat_collect(path, key, client,
    inventory$page_size, inventory$pagination, inventory$max_pages)
  # Verify project context and subject membership independently of optional fields.
  for (project in unique(selected$project_id)) {
    current <- collect(xnat_path("data/projects", url_encode(project), "experiments"))
    for (j in which(selected$project_id == project)) {
      index <- match(selected$experiment_id[j], inventory_text(current, "ID"))
      if (is.na(index)) inventory_abort("Selected session is no longer in its project.")
      metadata <- c(label = "label", date = "date", source_project_id = "project", xsi_type = "xsiType")
      for (field in names(metadata)) {
        if (!identical(selected[[field]][j], inventory_text(current, metadata[[field]])[index])) inventory_abort("Selected session metadata changed; refresh and review the inventory.")
      }
      reported <- inventory_text(current, "subject_ID")[index]
      if (!is.na(reported) && reported != selected$subject_id[j]) inventory_abort("Selected session subject changed.")
    }
    for (subject in unique(selected$subject_id[selected$project_id == project])) {
      members <- collect(xnat_path("data/projects", url_encode(project), "subjects", url_encode(subject), "experiments"))
      expected <- selected$experiment_id[selected$project_id == project & selected$subject_id == subject]
      if (any(!expected %in% inventory_text(members, "ID"))) inventory_abort("Could not verify selected subject/session membership.")
    }
  }
  resources <- files <- list()
  for (eid in experiment_ids) {
    project <- selected$project_id[match(eid, selected$experiment_id)]
    prefix <- xnat_path("data/experiments", url_encode(eid), "scans")
    current <- collect(prefix)
    previous <- inventory$scans[inventory$scans$experiment_id == eid & inventory$scans$project_id == project, , drop = FALSE]
    if (!setequal(inventory_text(current, "ID"), previous$scan_id)) inventory_abort("Session scan identities changed; refresh the inventory.")
    for (j in which(selection$experiment_id == eid)) {
      scan <- selection$scan_id[j]
      index <- match(scan, inventory_text(current, "ID"))
      for (field in c("type", "series_description", "quality")) {
        if (!identical(selection[[field]][j], inventory_text(current, field)[index])) inventory_abort("Selected scan metadata changed; refresh and review the inventory.")
      }
      current_resources <- collect(xnat_path(prefix, url_encode(scan), "resources"), "xnat_abstractresource_id")
      previous_resources <- inventory$resources[inventory$resources$project_id == project &
        inventory$resources$experiment_id == eid & inventory$resources$scan_id == scan, , drop = FALSE]
      ids <- inventory_text(current_resources, "xnat_abstractresource_id")
      if (!setequal(ids, previous_resources$resource_id) ||
          !identical(inventory_text(current_resources, "label"), previous_resources$label[match(ids, previous_resources$resource_id)])) {
        inventory_abort("Scan resource identities changed; refresh the inventory.")
      }
      dicom <- which(!is.na(inventory_text(current_resources, "label")) & inventory_text(current_resources, "label") == "DICOM")
      for (k in dicom) {
        rid <- ids[k]
        path <- plan_resource_path(eid, scan, rid)
        catalog <- collect(path, "URI")
        names <- inventory_text(catalog, "Name")
        uris <- inventory_text(catalog, "URI")
        sizes <- decimal_values(inventory_text(catalog, "Size"))
        if (!nrow(catalog) || anyNA(names) || anyNA(uris) || anyNA(sizes)) inventory_abort("DICOM catalog is empty or has unknown names, URIs or byte sizes.")
        if (any(grepl("[\r\n]", names)) || any(grepl("[?#\r\n]", uris)) || any(!startsWith(uris, "/")) || any(startsWith(uris, "//"))) inventory_abort("Invalid file catalog paths.")
        order <- order(uris)
        catalog <- tibble::tibble(name = names[order], uri = uris[order], bytes = sizes[order])
        expected_bytes <- decimal_sum(catalog$bytes)
        reported_count <- decimal_values(inventory_text(current_resources, "file_count")[k])
        reported_bytes <- decimal_values(inventory_text(current_resources, "file_size")[k])
        if ((!is.na(reported_count) && reported_count != as.character(nrow(catalog))) ||
            (!is.na(reported_bytes) && reported_bytes != expected_bytes)) inventory_abort("DICOM catalog disagrees with resource counts or byte sizes.")
        key <- plan_resource_key(project, eid, scan, rid)
        resources[[length(resources) + 1L]] <- tibble::tibble(project_id = project,
          experiment_id = eid, scan_id = scan, resource_id = rid, label = "DICOM",
          resource_key = key, path = path, expected_files = nrow(catalog), expected_bytes = expected_bytes,
          catalog_sha256 = plan_hash(catalog), destination = file.path(root, paste0(key, ".zip")))
        catalog$resource_key <- key
        files[[length(files) + 1L]] <- catalog
        selection$disposition[j] <- "included"
      }
    }
  }
  resource_table <- inventory_bind(resources)
  if (!nrow(resource_table) || !setequal(resource_table$experiment_id, experiment_ids)) inventory_abort("Every selected session must contain at least one non-empty DICOM resource.")
  plan <- structure(list(schema_version = 1L, server_url = inventory$server_url,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    inventory_captured_at = inventory$captured_at, destination_root = root,
    sessions = selected, scans = selection, resources = resource_table, files = inventory_bind(files),
    expected_resources = nrow(resource_table), expected_files = sum(resource_table$expected_files),
    expected_bytes = decimal_sum(resource_table$expected_bytes)), class = "xnat_download_plan")
  validate_download_plan(plan)
  plan
}

# IDs never become filesystem syntax, and case-distinct IDs remain distinct on
# case-insensitive volumes. Segment prefixes also avoid reserved device names.
plan_id <- function(x) vapply(x, function(value) paste0("id-", paste(format(charToRaw(enc2utf8(value))), collapse = "")), character(1), USE.NAMES = FALSE)
plan_resource_key <- function(project, experiment, scan, resource) {
  file.path(plan_id(project), plan_id(experiment), paste0(plan_id(scan), "-", plan_id(resource)))
}
plan_resource_path <- function(experiment, scan, resource) {
  xnat_path("data/experiments", url_encode(experiment), "scans", url_encode(scan), "resources", url_encode(resource), "files")
}
plan_root <- function(path) {
  check_string(path, "dest_dir")
  path <- gsub("\\", "/", path.expand(path), fixed = TRUE)
  if (!grepl("^(/|[A-Za-z]:/)", path)) path <- file.path(getwd(), path)
  if (any(strsplit(path, "/", fixed = TRUE)[[1]] == "..") || grepl("[\r\n]", path)) inventory_abort("Destination root must not contain parent traversal or newlines.")
  path <- gsub("/\\./", "/", path)
  path <- sub("/+$", "", path)
  if (!nzchar(path)) path <- "/"
  path
}
plan_keys <- function(table, columns) {
  if (!all(columns %in% names(table))) inventory_abort("Plan table is missing required identity columns.")
  values <- lapply(columns, function(column) {
    value <- inventory_text(table, column)
    if (anyNA(value)) inventory_abort("Plan contains unknown canonical identities.")
    plan_id(value)
  })
  do.call(paste, c(values, sep = "/"))
}
# Byte sizes remain decimal strings, including values above R's exact-double limit.
decimal_values <- function(x) {
  if (any(!is.na(x) & !grepl("^[0-9]+$", x))) inventory_abort("Byte sizes and counts must be non-negative decimal integers.")
  value <- sub("^0+", "", x)
  value[!is.na(value) & value == ""] <- "0"
  value
}
decimal_sum <- function(x) {
  x <- decimal_values(x)
  if (anyNA(x)) return(NA_character_)
  total <- 0L
  for (value in x) {
    digits <- rev(as.integer(strsplit(value, "", fixed = TRUE)[[1]]))
    length(total) <- length(digits) <- max(length(total), length(digits))
    total[is.na(total)] <- 0L
    digits[is.na(digits)] <- 0L
    total <- total + digits
    for (i in seq_along(total)) {
      if (total[i] >= 10L) {
        if (i == length(total)) total <- c(total, 0L)
        total[i + 1L] <- total[i + 1L] + total[i] %/% 10L
        total[i] <- total[i] %% 10L
      }
    }
  }
  paste(rev(total), collapse = "")
}
plan_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA, dataframe = "rows", pretty = FALSE)
plan_hash <- function(x) digest::digest(as.character(plan_json(x)), algo = "sha256", serialize = FALSE)

validate_download_plan <- function(plan) {
  if (!is.list(plan) || !is.numeric(plan$schema_version) || length(plan$schema_version) != 1L || !isTRUE(plan$schema_version == 1L)) inventory_abort("Unsupported download plan schema.")
  validate_server_url(plan$server_url)
  if (!identical(plan_root(plan$destination_root), plan$destination_root)) inventory_abort("Noncanonical destination root.")
  for (table in c("sessions", "scans", "resources", "files")) {
    if (!is.data.frame(plan[[table]]) || !nrow(plan[[table]])) inventory_abort("Plan contains empty or invalid tables.")
  }
  fields <- c("schema_version", "server_url", "created_at", "inventory_captured_at",
    "destination_root", "sessions", "scans", "resources", "files",
    "expected_resources", "expected_files", "expected_bytes")
  if (!setequal(names(plan), fields) || anyDuplicated(names(plan))) inventory_abort("Unexpected or missing fields in download plan.")
  schemas <- list(
    sessions = c(names(inventory_empty("sessions")), "participant_session_count"),
    scans = c(names(inventory_empty("scans")), "disposition"),
    resources = c("project_id", "experiment_id", "scan_id", "resource_id", "label",
      "resource_key", "path", "expected_files", "expected_bytes", "catalog_sha256", "destination"),
    files = c("name", "uri", "bytes", "resource_key"))
  for (table in names(schemas)) {
    if (!setequal(names(plan[[table]]), schemas[[table]]) || anyDuplicated(names(plan[[table]]))) inventory_abort("Unexpected or missing columns in plan table.")
    for (column in schemas[[table]]) {
      value <- plan[[table]][[column]]
      if (column == "series_match") {
        if (!is.logical(value)) inventory_abort("Invalid series_match type.")
      } else if (column %in% c("scan_count", "participant_session_count", "expected_files")) {
        if (!is.numeric(value) || any(!is.na(value) & (!is.finite(value) | value < 0 | value != floor(value)))) inventory_abort("Invalid count in plan table.")
      } else if (!is.character(value)) inventory_abort("Plan identity and metadata columns must be strings.")
    }
  }
  for (field in c("expected_resources", "expected_files")) {
    value <- plan[[field]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) || value < 1 || value != floor(value)) inventory_abort("Invalid plan total.")
  }
  for (field in c("created_at", "inventory_captured_at", "expected_bytes")) check_string(plan[[field]], field)
  s <- plan$sessions; c <- plan$scans; r <- plan$resources; f <- plan$files
  if (anyNA(s$subject_status) || any(!s$subject_status %in% c("reported", "resolved"))) inventory_abort("Selected subjects must be resolved or reported.")
  session_keys <- plan_keys(s, c("project_id", "experiment_id", "subject_id"))
  if (anyDuplicated(s$experiment_id) || anyDuplicated(session_keys)) inventory_abort("Plan has ambiguous session identities.")
  scan_keys <- plan_keys(c, c("project_id", "experiment_id", "scan_id"))
  if (anyDuplicated(scan_keys) || anyNA(c$disposition) || any(!c$disposition %in% c("included", "no_dicom"))) inventory_abort("Invalid scan selection.")
  if (any(!plan_keys(c, c("project_id", "experiment_id")) %in% plan_keys(s, c("project_id", "experiment_id")))) inventory_abort("Scan selection is outside selected sessions.")
  keys <- plan_keys(r, c("project_id", "experiment_id", "scan_id", "resource_id"))
  if (anyDuplicated(keys) || anyNA(r$label) || any(r$label != "DICOM") ||
      any(!plan_keys(r, c("project_id", "experiment_id", "scan_id")) %in% scan_keys)) inventory_abort("Invalid DICOM resource selection.")
  if (!setequal(r$experiment_id, s$experiment_id)) inventory_abort("Session selection is not covered by resources.")
  expected_disposition <- ifelse(scan_keys %in% plan_keys(r, c("project_id", "experiment_id", "scan_id")), "included", "no_dicom")
  if (!identical(c$disposition, expected_disposition)) inventory_abort("Scan dispositions disagree with resource selection.")
  if (anyNA(f$resource_key) || any(!f$resource_key %in% r$resource_key)) inventory_abort("File catalog is outside selected resources.")
  for (i in seq_len(nrow(r))) {
    key <- plan_resource_key(r$project_id[i], r$experiment_id[i], r$scan_id[i], r$resource_id[i])
    path <- plan_resource_path(r$experiment_id[i], r$scan_id[i], r$resource_id[i])
    if (any(nchar(strsplit(key, "/", fixed = TRUE)[[1]], type = "bytes") > 220L)) inventory_abort("Canonical IDs exceed safe destination component lengths.")
    if (!identical(r$resource_key[i], key) || !identical(r$path[i], path) ||
        !identical(r$destination[i], file.path(plan$destination_root, paste0(key, ".zip")))) inventory_abort("Resource paths disagree with canonical identities.")
    catalog <- f[f$resource_key == key, c("name", "uri", "bytes"), drop = FALSE]
    if (!nrow(catalog) || anyNA(catalog) || anyDuplicated(catalog$uri) ||
        any(!nzchar(catalog$name)) || any(grepl("[\r\n]", catalog$name)) ||
        any(!startsWith(catalog$uri, "/")) || any(startsWith(catalog$uri, "//")) ||
        any(grepl("[?#\r\n]", catalog$uri))) inventory_abort("Invalid file catalog.")
    if (!identical(catalog$bytes, decimal_values(catalog$bytes)) ||
        !identical(r$catalog_sha256[i], plan_hash(catalog)) ||
        !isTRUE(r$expected_files[i] == nrow(catalog)) ||
        !identical(r$expected_bytes[i], decimal_sum(catalog$bytes))) inventory_abort("Catalog counts, bytes or fingerprints disagree.")
  }
  if (anyDuplicated(tolower(r$destination)) || !isTRUE(plan$expected_resources == nrow(r)) ||
      !isTRUE(plan$expected_files == nrow(f)) || !identical(plan$expected_bytes, decimal_sum(f$bytes))) inventory_abort("Plan totals or destinations are inconsistent.")
  invisible(TRUE)
}

#' Save and read a frozen download plan
#'
#' JSON contains only selection metadata, never the client or credentials. A
#' SHA-256 fingerprint detects accidental changes. This is not a signature or a
#' remote-source checksum. Treat catalogs as potentially sensitive research data.
#'
#' @param plan An xnat_download_plan.
#' @param path JSON plan file. Parent directory must exist.
#' @param overwrite Whether to replace an existing file; defaults to FALSE.
#' @return xnat_write_plan invisibly returns the path; xnat_read_plan returns a
#'   validated xnat_download_plan without contacting the server.
#' @export
xnat_write_plan <- function(plan, path, overwrite = FALSE) {
  validate_download_plan(plan)
  check_string(path, "path")
  if (!isTRUE(overwrite) && file.exists(path)) inventory_abort("Plan file already exists.")
  if (!dir.exists(dirname(path))) inventory_abort("Plan parent directory does not exist.")
  payload <- unclass(plan)
  envelope <- list(plan = payload, sha256 = plan_hash(payload))
  tmp <- tempfile(".xnatr-plan-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp, showWarnings = FALSE)
  Sys.chmod(tmp, mode = "0600")
  writeLines(jsonlite::prettify(plan_json(envelope)), tmp, useBytes = TRUE)
  # A same-directory hard link atomically refuses an existing destination.
  ok <- if (isTRUE(overwrite)) file.rename(tmp, path) else suppressWarnings(file.link(tmp, path))
  if (!ok) inventory_abort("Could not publish the plan file.")
  invisible(path)
}

#' @rdname xnat_write_plan
#' @export
xnat_read_plan <- function(path) {
  check_string(path, "path")
  if (!file.exists(path) || dir.exists(path)) inventory_abort("Plan file does not exist.")
  envelope <- jsonlite::fromJSON(path, simplifyVector = FALSE, bigint_as_char = TRUE)
  if (!is.list(envelope) || !is.list(envelope$plan) ||
      !identical(envelope$sha256, plan_hash(envelope$plan))) inventory_abort("Plan fingerprint mismatch or invalid plan file.")
  plan <- envelope$plan
  for (table in c("sessions", "scans", "resources", "files")) {
    records <- plan[[table]]
    if (!is.list(records) || !length(records)) inventory_abort("Invalid plan table.")
    rows <- lapply(records, function(row) {
      row <- lapply(row, function(value) if (is.null(value)) NA else value)
      tibble::as_tibble(row)
    })
    plan[[table]] <- inventory_bind(rows)
    # All-null optional fields have no inferable type in JSON.
    numeric_columns <- c("scan_count", "participant_session_count", "expected_files")
    for (column in setdiff(names(plan[[table]]), c(numeric_columns, "series_match"))) {
      plan[[table]][[column]] <- as.character(plan[[table]][[column]])
    }
  }
  class(plan) <- "xnat_download_plan"
  validate_download_plan(plan)
  plan
}

#' @export
print.xnat_download_plan <- function(x, ...) {
  cat(sprintf("DICOM plan: %d sessions, %d resources, %d files, %s uncompressed bytes\n",
    nrow(x$sessions), x$expected_resources, x$expected_files, x$expected_bytes))
  cat("Destination:", x$destination_root, "\n")
  print(x$resources, ...)
  invisible(x)
}

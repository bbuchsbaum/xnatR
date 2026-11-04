#' Discover sessions, scans and resources across accessible projects
#'
#' Traverses complete collections, retaining canonical IDs and explicit unknowns.
#' Missing subject IDs are resolved by enumerating subject-scoped experiments,
#' never by interpreting labels. Any request or pagination failure aborts the
#' inventory rather than returning a partial success. Results describe accessible
#' data at query time, not an atomic server snapshot.
#'
#' @param project_ids Canonical project IDs; NULL discovers all accessible projects.
#' @param date_from,date_to Inclusive ISO dates or Date values. Unknown dates are
#'   retained unless `include_unknown_dates = FALSE`.
#' @param series_pattern Optional case-insensitive regular expression matching scan
#'   type or series description. Retains matching sessions and all their scans.
#' @param include_unknown_dates Whether to retain sessions with unknown dates.
#' @param client Optional xnat_client; defaults to the current global client.
#' @param page_size Requested rows per page.
#' @param pagination `auto` tries pages and checks a legacy `limit=*` fallback
#'   when paging is ignored. `paged` refuses ignored paging; `all` requires an
#'   unpaged response whose totalRecords matches the returned row count.
#' @param max_pages Maximum requests per paged collection before refusing completion.
#' @return An `xnat_inventory` list of projects, subjects, sessions, scans,
#'   scan resources, filters and collection traversal evidence. Participant session
#'   counts refer to retained sessions within each project. Resource coverage is
#'   scan-level, not experiment- or assessor-level. No file bytes are downloaded.
#' @export
xnat_inventory <- function(project_ids = NULL, date_from = NULL, date_to = NULL,
                           series_pattern = NULL, include_unknown_dates = TRUE,
                           client = NULL, page_size = 100L,
                           pagination = c("auto", "paged", "all"), max_pages = 10000L) {
  client <- client %||% xnat_current_client()
  require_auth(client)
  pagination <- match.arg(pagination)
  lower <- inventory_date_bound(date_from)
  upper <- inventory_date_bound(date_to)
  if (!is.null(lower) && !is.null(upper) && lower > upper) inventory_abort("date_from exceeds date_to.")
  if (!is.logical(include_unknown_dates) || length(include_unknown_dates) != 1L || is.na(include_unknown_dates)) inventory_abort("include_unknown_dates must be TRUE or FALSE.")
  if (!is.null(series_pattern)) {
    check_string(series_pattern, "series_pattern")
    grepl(series_pattern, "", ignore.case = TRUE)
  }
  audit <- list()
  collect <- function(path, key = "ID", query = list()) {
    rows <- xnat_collect(path, key, client, page_size, pagination, max_pages, query)
    audit[[length(audit) + 1L]] <<- attr(rows, "pagination")
    attr(rows, "pagination") <- NULL
    rows
  }
  raw_projects <- collect("data/projects", query = list(columns = "ID,name"))
  projects <- tibble::tibble(project_id = inventory_text(raw_projects, "ID"),
    name = inventory_text(raw_projects, "name"))
  if (!is.null(project_ids)) {
    inventory_selection(project_ids, "project_ids")
    if (any(!project_ids %in% projects$project_id)) inventory_abort("Some requested projects are not accessible canonical project IDs.")
    projects <- projects[match(project_ids, projects$project_id), , drop = FALSE]
  }
  subject_tables <- session_tables <- scan_tables <- resource_tables <- list()
  for (project in projects$project_id) {
    root <- xnat_path("data/projects", url_encode(project))
    raw_subjects <- collect(xnat_path(root, "subjects"))
    subjects <- tibble::tibble(project_id = rep(project, nrow(raw_subjects)),
      subject_id = inventory_text(raw_subjects, "ID"), label = inventory_text(raw_subjects, "label"))
    subject_tables[[length(subject_tables) + 1L]] <- subjects
    raw_sessions <- collect(xnat_path(root, "experiments"))
    sessions <- tibble::tibble(project_id = rep(project, nrow(raw_sessions)),
      experiment_id = inventory_text(raw_sessions, "ID"),
      label = inventory_text(raw_sessions, "label"),
      source_project_id = inventory_text(raw_sessions, "project"),
      subject_id = inventory_text(raw_sessions, "subject_ID"),
      date = inventory_text(raw_sessions, "date"), xsi_type = inventory_text(raw_sessions, "xsiType"))
    dates <- inventory_dates(sessions$date)
    sessions$date_status <- ifelse(is.na(dates), "unknown", "known")
    keep <- rep(TRUE, nrow(sessions))
    if (!is.null(lower)) keep <- keep & (is.na(dates) | dates >= lower)
    if (!is.null(upper)) keep <- keep & (is.na(dates) | dates <= upper)
    if (!include_unknown_dates) keep <- keep & !is.na(dates)
    sessions <- sessions[keep, , drop = FALSE]
    sessions$subject_status <- ifelse(!is.na(sessions$subject_id) & sessions$subject_id %in% subjects$subject_id, "reported", "unknown")
    if (any(sessions$subject_status == "unknown")) {
      associations <- list()
      for (sid in subjects$subject_id) {
        membership <- collect(xnat_path(root, "subjects", url_encode(sid), "experiments"))
        associations[[length(associations) + 1L]] <- tibble::tibble(
          experiment_id = inventory_text(membership, "ID"), subject_id = rep(sid, nrow(membership)))
      }
      membership <- inventory_bind(associations, tibble::tibble(experiment_id = character(), subject_id = character()))
      for (j in seq_len(nrow(sessions))) {
        ids <- unique(membership$subject_id[membership$experiment_id == sessions$experiment_id[j]])
        reported <- sessions$subject_id[j]
        if (length(ids) > 1L || (length(ids) == 1L && !is.na(reported) && reported != ids)) {
          inventory_abort("Conflicting canonical subject associations; refresh or resolve server metadata.")
        }
        if (length(ids) == 1L) {
          sessions$subject_id[j] <- ids
          sessions$subject_status[j] <- "resolved"
        } else if (sessions$subject_status[j] == "unknown") {
          sessions$subject_id[j] <- NA_character_
        }
      }
    }
    for (j in seq_len(nrow(sessions))) {
      eid <- sessions$experiment_id[j]
      session_root <- xnat_path("data/experiments", url_encode(eid))
      raw_scans <- collect(xnat_path(session_root, "scans"))
      scans <- tibble::tibble(project_id = rep(project, nrow(raw_scans)),
        experiment_id = rep(eid, nrow(raw_scans)), scan_id = inventory_text(raw_scans, "ID"),
        type = inventory_text(raw_scans, "type"),
        series_description = inventory_text(raw_scans, "series_description"),
        quality = inventory_text(raw_scans, "quality"))
      scans$series_match <- if (is.null(series_pattern)) rep(NA, nrow(scans)) else
        (!is.na(scans$type) & grepl(series_pattern, scans$type, ignore.case = TRUE)) |
        (!is.na(scans$series_description) & grepl(series_pattern, scans$series_description, ignore.case = TRUE))
      if (!is.null(series_pattern) && !any(scans$series_match)) next
      scan_tables[[length(scan_tables) + 1L]] <- scans
      for (scan in scans$scan_id) {
        raw_resources <- collect(xnat_path(session_root, "scans", url_encode(scan), "resources"), "xnat_abstractresource_id")
        resource_tables[[length(resource_tables) + 1L]] <- tibble::tibble(
          project_id = rep(project, nrow(raw_resources)), experiment_id = rep(eid, nrow(raw_resources)),
          scan_id = rep(scan, nrow(raw_resources)), resource_id = inventory_text(raw_resources, "xnat_abstractresource_id"),
          label = inventory_text(raw_resources, "label"), format = inventory_text(raw_resources, "format"),
          reported_file_count = inventory_text(raw_resources, "file_count"),
          reported_bytes = inventory_text(raw_resources, "file_size"))
      }
      row <- sessions[j, , drop = FALSE]
      row$scan_count <- nrow(scans)
      row$scan_types <- inventory_summary(scans$type)
      row$series_descriptions <- inventory_summary(scans$series_description)
      session_tables[[length(session_tables) + 1L]] <- row
    }
  }
  sessions <- inventory_bind(session_tables, inventory_empty("sessions"))
  sessions$participant_session_count <- vapply(seq_len(nrow(sessions)), function(j) {
    if (is.na(sessions$subject_id[j])) return(NA_integer_)
    as.integer(sum(sessions$project_id == sessions$project_id[j] &
      !is.na(sessions$subject_id) & sessions$subject_id == sessions$subject_id[j]))
  }, integer(1))
  structure(list(schema_version = 1L, server_url = client$base_url,
    captured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    filters = list(project_ids = project_ids, date_from = as.character(lower), date_to = as.character(upper),
      series_pattern = series_pattern, include_unknown_dates = include_unknown_dates),
    projects = projects, subjects = inventory_bind(subject_tables, inventory_empty("subjects")),
    sessions = sessions, scans = inventory_bind(scan_tables, inventory_empty("scans")),
    resources = inventory_bind(resource_tables, inventory_empty("resources")),
    collections = audit, page_size = page_size, pagination = pagination, max_pages = max_pages),
    class = "xnat_inventory")
}

inventory_dates <- function(x) {
  valid <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  result <- as.Date(rep(NA_character_, length(x)))
  result[valid] <- suppressWarnings(as.Date(x[valid], format = "%Y-%m-%d"))
  result[!is.na(result) & as.character(result) != x] <- as.Date(NA)
  result
}

inventory_date_bound <- function(x) {
  if (is.null(x)) return(NULL)
  if (length(x) != 1L) inventory_abort("Date bounds must be single ISO dates.")
  date <- inventory_dates(as.character(x))
  if (is.na(date)) inventory_abort("Date bounds must be valid ISO dates.")
  date
}

inventory_summary <- function(x) {
  values <- sort(unique(x[!is.na(x)]))
  if (!length(values)) NA_character_ else paste(values, collapse = "; ")
}

inventory_selection <- function(x, name) {
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x)) {
    inventory_abort(paste0(name, " must contain unique, non-empty canonical IDs."))
  }
}

inventory_empty <- function(table) {
  columns <- switch(table,
    subjects = c("project_id", "subject_id", "label"),
    sessions = c("project_id", "experiment_id", "label", "source_project_id", "subject_id", "date", "xsi_type", "date_status", "subject_status", "scan_types", "series_descriptions"),
    scans = c("project_id", "experiment_id", "scan_id", "type", "series_description", "quality"),
    resources = c("project_id", "experiment_id", "scan_id", "resource_id", "label", "format", "reported_file_count", "reported_bytes"))
  result <- tibble::as_tibble(stats::setNames(rep(list(character()), length(columns)), columns))
  if (table == "sessions") result$scan_count <- integer()
  if (table == "scans") result$series_match <- logical()
  result
}

#' @export
print.xnat_inventory <- function(x, ...) {
  cat(sprintf("XNAT inventory: %d projects, %d subjects, %d sessions, %d scans, %d scan resources\n",
    nrow(x$projects), nrow(x$subjects), nrow(x$sessions), nrow(x$scans), nrow(x$resources)))
  print(x$sessions, ...)
  invisible(x)
}

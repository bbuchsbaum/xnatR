#' List Experiments (Sessions) for a Project or Subject
#'
#' Retrieves a list of experiments (imaging sessions). When only
#' `project_id` is given, returns all experiments across every subject in
#' that project. When `subject_id` is also supplied, results are scoped to
#' that single subject.
#'
#' @param project_id The project identifier (required).
#' @param subject_id Optional subject identifier. When `NULL`, all
#'   experiments in the project are returned.
#' @param columns Character vector of column names to include.
#' @param limit Maximum number of results to return.
#' @param offset Number of results to skip for pagination.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_experiments` containing experiment details.
#'   Common columns include:
#'   - `ID`: Experiment identifier
#'   - `label`: Experiment label
#'   - `project`: Project ID
#'   - `subject_ID`: Subject ID
#'   - `xsiType`: Experiment type (e.g., "xnat:mrSessionData")
#'   - `date`: Session date
#'
#' @examples
#' \dontrun{
#' authenticate_xnat(base_url = "https://central.xnat.org",
#'                   username = "guest", password = "guest")
#'
#' # All experiments in a project
#' all_exps <- list_experiments(project_id = "MyProject")
#'
#' # Experiments for one subject
#' subj_exps <- list_experiments(
#'   project_id = "MyProject",
#'   subject_id = "Subject001"
#' )
#' }
#'
#' @export
list_experiments <- function(project_id, subject_id = NULL, columns = NULL, limit = NULL, offset = NULL, client = NULL) {
  check_string(project_id, "project_id")

  if (!is.null(subject_id)) {
    check_string(subject_id, "subject_id")
    path <- xnat_path(
      "data/projects", url_encode(project_id),
      "subjects", url_encode(subject_id),
      "experiments"
    )
  } else {
    path <- xnat_path(
      "data/projects", url_encode(project_id),
      "experiments"
    )
  }

  query <- list(
    columns = if (!is.null(columns)) paste(columns, collapse = ",") else NULL,
    limit = limit,
    offset = offset
  )

  result <- xnat_get_tibble(path, query = query, class_name = "xnat_experiments", client = client)
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  result
}

#' List Recent Sessions for a Project
#'
#' Convenience wrapper around [list_experiments()] that returns experiments
#' sorted by date (most recent first) with an optional cap on the number of
#' rows returned.
#'
#' @param project_id The project identifier (required).
#' @param n Maximum number of sessions to return. `NULL` (the default)
#'   returns all sessions.
#' @param subject_id Optional subject identifier to scope results.
#' @param columns Character vector of column names to include.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_experiments`, ordered most-recent first.
#'
#' @examples
#' \dontrun{
#' # 10 most recent sessions in a project
#' recent <- list_recent_sessions("MyProject", n = 10)
#'
#' # All sessions for a subject, newest first
#' recent_subj <- list_recent_sessions("MyProject", subject_id = "S001")
#' }
#'
#' @export
list_recent_sessions <- function(project_id, n = NULL, subject_id = NULL, columns = NULL, client = NULL) {
  exps <- list_experiments(
    project_id = project_id,
    subject_id = subject_id,
    columns = columns,
    client = client
  )

  if (nrow(exps) == 0) {
    return(exps)
  }

  # Sort by date descending when a date column is present
  if ("date" %in% names(exps)) {
    exps <- exps[order(exps$date, decreasing = TRUE), , drop = FALSE]
  }

  if (!is.null(n)) {
    n <- min(n, nrow(exps))
    exps <- exps[seq_len(n), , drop = FALSE]
  }

  exps
}

#' @export
print.xnat_experiments <- function(x, ...) {
  n <- nrow(x)
  project <- attr(x, "project_id")
  subject <- attr(x, "subject_id")

  cli::cli_h1("XNAT Experiments")
  if (!is.null(project)) {
    cli::cli_text("Project: {.val {project}}")
  }
  if (!is.null(subject)) {
    cli::cli_text("Subject: {.val {subject}}")
  }
  cli::cli_text("{n} experiment{?s}")

  if (n > 0) {
    display_cols <- intersect(c("ID", "label", "xsiType", "date"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

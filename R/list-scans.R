#' List Scans for an Experiment
#'
#' Retrieves a list of scans from a specific experiment.
#'
#' @param project_id The project identifier (required).
#' @param subject_id The subject identifier (required).
#' @param experiment_id The experiment identifier (required).
#' @param columns Character vector of column names to include.
#' @param limit Maximum number of results to return.
#' @param offset Number of results to skip for pagination.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_scans` containing scan details.
#'   Common columns include:
#'   - `ID`: Scan number
#'   - `type`: Scan type (e.g., "T1", "T2", "BOLD")
#'   - `series_description`: Series description
#'   - `quality`: Scan quality rating
#'   - `xsiType`: Scan data type
#'   - `frames`: Number of frames
#'
#' @examples
#' \dontrun{
#' authenticate_xnat(base_url = "https://central.xnat.org",
#'                   username = "guest", password = "guest")
#'
#' scans <- list_scans(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Experiment001"
#' )
#' }
#'
#' @export
list_scans <- function(project_id, subject_id, experiment_id,
                       columns = NULL, limit = NULL, offset = NULL, client = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "scans"
  )

  query <- list(
    columns = if (!is.null(columns)) paste(columns, collapse = ",") else NULL,
    limit = limit,
    offset = offset
  )

  result <- xnat_get_tibble(path, query = query, class_name = "xnat_scans", client = client)
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  attr(result, "experiment_id") <- experiment_id
  result
}

#' @export
print.xnat_scans <- function(x, ...) {
  n <- nrow(x)
  project <- attr(x, "project_id")
  subject <- attr(x, "subject_id")
  experiment <- attr(x, "experiment_id")

  cli::cli_h1("XNAT Scans")
  if (!is.null(project)) {
    cli::cli_text("Project: {.val {project}}")
  }
  if (!is.null(subject)) {
    cli::cli_text("Subject: {.val {subject}}")
  }
  if (!is.null(experiment)) {
    cli::cli_text("Experiment: {.val {experiment}}")
  }
  cli::cli_text("{n} scan{?s}")

  if (n > 0) {
    display_cols <- intersect(c("ID", "type", "series_description", "quality"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

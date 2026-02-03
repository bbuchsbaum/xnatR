#' List Experiments for a Subject
#'
#' Retrieves a list of experiments (imaging sessions) for a specific subject.
#'
#' @param project_id The project identifier (required).
#' @param subject_id The subject identifier (required).
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
#' experiments <- list_experiments(
#'   project_id = "MyProject",
#'   subject_id = "Subject001"
#' )
#' }
#'
#' @export
list_experiments <- function(project_id, subject_id, columns = NULL, limit = NULL, offset = NULL, client = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments"
  )

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

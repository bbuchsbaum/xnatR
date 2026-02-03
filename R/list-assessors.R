#' List Assessors for an Experiment
#'
#' Retrieves assessors (derived data like FreeSurfer outputs, QC results)
#' for a specific experiment.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param experiment_id The experiment identifier.
#' @param columns Character vector of column names to include.
#' @param limit Maximum number of results to return.
#' @param offset Number of results to skip for pagination.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_assessors` containing assessor details.
#'   Common columns include:
#'   - `ID`: Assessor identifier
#'   - `label`: Assessor label
#'   - `xsiType`: Assessor data type
#'   - `project`: Project ID
#'
#' @examples
#' \dontrun{
#' assessors <- list_assessors(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001"
#' )
#' }
#'
#' @export
list_assessors <- function(project_id, subject_id, experiment_id,
                           columns = NULL, limit = NULL, offset = NULL, client = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "assessors"
  )

  query <- list(
    columns = if (!is.null(columns)) paste(columns, collapse = ",") else NULL,
    limit = limit,
    offset = offset
  )

  result <- xnat_get_tibble(path, query = query, class_name = "xnat_assessors", client = client)
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  attr(result, "experiment_id") <- experiment_id
  result
}

#' @export
print.xnat_assessors <- function(x, ...) {
  n <- nrow(x)
  experiment <- attr(x, "experiment_id")

  cli::cli_h1("XNAT Assessors")
  if (!is.null(experiment)) {
    cli::cli_text("Experiment: {.val {experiment}}")
  }
  cli::cli_text("{n} assessor{?s}")

  if (n > 0) {
    display_cols <- intersect(c("ID", "label", "xsiType"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

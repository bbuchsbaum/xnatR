#' List Reconstructions for an Experiment
#'
#' Retrieves reconstructions (processed image data) for a specific experiment.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param experiment_id The experiment identifier.
#' @param columns Character vector of column names to include.
#' @param limit Maximum number of results to return.
#' @param offset Number of results to skip for pagination.
#'
#' @return A tibble of class `xnat_reconstructions` containing reconstruction details.
#'   Common columns include:
#'   - `ID`: Reconstruction identifier
#'   - `type`: Reconstruction type
#'   - `xsiType`: Data type
#'
#' @examples
#' \dontrun{
#' recons <- list_reconstructions(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001"
#' )
#' }
#'
#' @export
list_reconstructions <- function(project_id, subject_id, experiment_id,
                                  columns = NULL, limit = NULL, offset = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "reconstructions"
  )

  query <- list(
    columns = if (!is.null(columns)) paste(columns, collapse = ",") else NULL,
    limit = limit,
    offset = offset
  )

  result <- xnat_get_tibble(path, query = query, class_name = "xnat_reconstructions")
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  attr(result, "experiment_id") <- experiment_id
  result
}

#' @export
print.xnat_reconstructions <- function(x, ...) {
  n <- nrow(x)
  experiment <- attr(x, "experiment_id")

  cli::cli_h1("XNAT Reconstructions")
  if (!is.null(experiment)) {
    cli::cli_text("Experiment: {.val {experiment}}")
  }
  cli::cli_text("{n} reconstruction{?s}")

  if (n > 0) {
    display_cols <- intersect(c("ID", "type", "xsiType"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

#' List Resources for a Scan
#'
#' Retrieves resources (e.g., DICOM, NIFTI) available for a specific scan.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param experiment_id The experiment identifier.
#' @param scan_id The scan identifier.
#'
#' @return A tibble of class `xnat_resources` containing resource details.
#'   Common columns include:
#'   - `xnat_abstractresource_id`: Resource ID
#'   - `label`: Resource label (e.g., "DICOM", "NIFTI")
#'   - `file_count`: Number of files
#'   - `file_size`: Total size in bytes
#'   - `format`: Resource format
#'
#' @examples
#' \dontrun{
#' resources <- list_resources(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001",
#'   scan_id = "1"
#' )
#' }
#'
#' @export
list_resources <- function(project_id, subject_id, experiment_id, scan_id) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")
  check_string(scan_id, "scan_id")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "scans", url_encode(scan_id),
    "resources"
  )

  result <- xnat_get_tibble(path, class_name = "xnat_resources")
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  attr(result, "experiment_id") <- experiment_id
  attr(result, "scan_id") <- scan_id
  result
}

#' List Resources for an Experiment
#'
#' Retrieves resources available at the experiment level.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param experiment_id The experiment identifier.
#'
#' @return A tibble of class `xnat_resources`.
#'
#' @examples
#' \dontrun{
#' resources <- list_experiment_resources(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001"
#' )
#' }
#'
#' @export
list_experiment_resources <- function(project_id, subject_id, experiment_id) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "resources"
  )

  result <- xnat_get_tibble(path, class_name = "xnat_resources")
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  attr(result, "experiment_id") <- experiment_id
  result
}

#' @export
print.xnat_resources <- function(x, ...) {
  n <- nrow(x)
  scan <- attr(x, "scan_id")

  cli::cli_h1("XNAT Resources")
  if (!is.null(scan)) {
    cli::cli_text("Scan: {.val {scan}}")
  }
  cli::cli_text("{n} resource{?s}")

  if (n > 0) {
    display_cols <- intersect(c("label", "file_count", "file_size", "format"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

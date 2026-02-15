#' List Files in a Resource
#'
#' Retrieves a list of files within a specific resource.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param experiment_id The experiment identifier.
#' @param scan_id The scan identifier.
#' @param resource The resource label (e.g., "DICOM", "NIFTI").
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_files` containing file details.
#'   Common columns include:
#'   - `Name`: File name
#'   - `Size`: File size in bytes
#'   - `URI`: File URI for downloading
#'   - `collection`: Resource collection name
#'   - `file_format`: File format
#'   - `file_content`: Content type
#'
#' @examples
#' \dontrun{
#' files <- list_files(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001",
#'   scan_id = "1",
#'   resource = "DICOM"
#' )
#' }
#'
#' @export
list_files <- function(project_id, subject_id, experiment_id, scan_id, resource, client = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")
  check_string(scan_id, "scan_id")
  check_string(resource, "resource")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "scans", url_encode(scan_id),
    "resources", url_encode(resource),
    "files"
  )

  result <- xnat_get_tibble(path, class_name = "xnat_files", client = client)
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  attr(result, "experiment_id") <- experiment_id
  attr(result, "scan_id") <- scan_id
  attr(result, "resource") <- resource
  result
}

#' List Files in an Experiment Resource
#'
#' Retrieves files from an experiment-level resource.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param experiment_id The experiment identifier.
#' @param resource The resource label.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_files`.
#'
#' @examples
#' \dontrun{
#' files <- list_experiment_files(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001",
#'   resource = "SNAPSHOTS"
#' )
#' }
#'
#' @export
list_experiment_files <- function(project_id, subject_id, experiment_id, resource, client = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")
  check_string(resource, "resource")

  path <- xnat_path(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "resources", url_encode(resource),
    "files"
  )

  result <- xnat_get_tibble(path, class_name = "xnat_files", client = client)
  attr(result, "project_id") <- project_id
  attr(result, "subject_id") <- subject_id
  attr(result, "experiment_id") <- experiment_id
  attr(result, "resource") <- resource
  result
}

#' List All Files for an Experiment
#'
#' Returns experiment-level files and, optionally, falls back to scan-level files
#' (`scans/ALL/files`) if none are present at the experiment level.
#'
#' @param experiment_id Experiment identifier.
#' @param include_scan_level If `TRUE` (default), fall back to `scans/ALL/files`
#'   when experiment-level files are empty.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_files`.
#' @export
list_experiment_files_all <- function(experiment_id, include_scan_level = TRUE, client = NULL) {
  check_string(experiment_id, "experiment_id")

  path <- xnat_path("data/experiments", url_encode(experiment_id), "files")
  result <- xnat_get_tibble(path, class_name = "xnat_files", client = client)

  if (isTRUE(include_scan_level) && nrow(result) == 0) {
    fallback_path <- xnat_path("data/experiments", url_encode(experiment_id), "scans", "ALL", "files")
    result <- xnat_get_tibble(fallback_path, class_name = "xnat_files", client = client)
  }

  attr(result, "experiment_id") <- experiment_id
  result
}

#' @export
print.xnat_files <- function(x, ...) {
  n <- nrow(x)
  resource <- attr(x, "resource")

  cli::cli_h1("XNAT Files")
  if (!is.null(resource)) {
    cli::cli_text("Resource: {.val {resource}}")
  }
  cli::cli_text("{n} file{?s}")

  if (n > 0) {
    display_cols <- intersect(c("Name", "Size", "file_format"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

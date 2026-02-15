#' Download Files from XNAT
#'
#' Downloads files from an XNAT experiment, with options to select specific
#' scans and resources.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param experiment_id The experiment identifier.
#' @param scan_id Scan ID(s) to download. Can be:
#'   - A single scan ID (e.g., "1")
#'   - Multiple IDs (e.g., c("1", "2", "3") or "1,2,3")
#'   - "ALL" to download all scans (default)
#' @param resource Resource name (e.g., "DICOM", "NIFTI"). NULL for all resources.
#' @param format Download format: "zip" (default) or "tar.gz".
#' @param dest_dir Destination directory. Defaults to current working directory.
#' @param dest_file Custom destination filename. If NULL, auto-generated.
#' @param progress Show download progress bar. Default TRUE.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return Invisibly returns the path to the downloaded file.
#'
#' @examples
#' \dontrun{
#' authenticate_xnat(base_url = "https://central.xnat.org",
#'                   username = "guest", password = "guest")
#'
#' # Download all scans as ZIP
#' download_files(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001"
#' )
#'
#' # Download specific scans
#' download_files(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001",
#'   scan_id = c("1", "2")
#' )
#'
#' # Download DICOM only
#' download_files(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   experiment_id = "Exp001",
#'   resource = "DICOM"
#' )
#' }
#'
#' @export
download_files <- function(project_id,
                           subject_id,
                           experiment_id,
                           scan_id = "ALL",
                           resource = NULL,
                           format = "zip",
                           dest_dir = getwd(),
                           dest_file = NULL,
                           progress = TRUE,
                           client = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")
  check_string(experiment_id, "experiment_id")

  # Normalize scan_id
  if (length(scan_id) > 1) {
    scan_id <- paste(scan_id, collapse = ",")
  }

  # Ensure destination directory exists
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Build path
  path_parts <- c(
    "data/projects", url_encode(project_id),
    "subjects", url_encode(subject_id),
    "experiments", url_encode(experiment_id),
    "scans", url_encode(scan_id)
  )

  if (!is.null(resource)) {
    path_parts <- c(path_parts, "resources", url_encode(resource))
  }

  path_parts <- c(path_parts, "files")
  path <- xnat_path(path_parts)

  # Generate filename if not provided
  if (is.null(dest_file)) {
    resource_label <- if (is.null(resource)) "ALL" else resource
    ext <- if (format == "tar.gz") ".tar.gz" else ".zip"
    dest_file <- sprintf("%s_%s_%s_%s_%s%s",
                         project_id, subject_id, experiment_id,
                         gsub(",", "-", scan_id), resource_label, ext)
  }

  dest_path <- file.path(dest_dir, dest_file)

  # Download
  cli::cli_progress_step("Downloading to {.file {dest_file}}")
  xnat_download(path, dest_path, query = list(format = format), progress = progress, client = client)
  cli::cli_alert_success("Downloaded to {.file {dest_path}}")

  invisible(dest_path)
}

#' Download a Single File from XNAT
#'
#' Downloads a file using its full URL or a pre-constructed path.
#'
#' @param url Full URL to the file, or a path relative to the XNAT base URL.
#' @param dest_file Destination file path.
#' @param progress Show download progress bar. Default TRUE.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return Invisibly returns the destination path.
#'
#' @examples
#' \dontrun{
#' download_xnat_file(
#'   url = "/data/projects/P1/subjects/S1/experiments/E1/scans/1/resources/DICOM/files/file.dcm",
#'   dest_file = "file.dcm"
#' )
#' }
#'
#' @export
download_xnat_file <- function(url, dest_file, progress = TRUE, client = NULL) {
  check_string(url, "url")
  check_string(dest_file, "dest_file")

  # If url starts with http, extract the path
  if (grepl("^https?://", url)) {
    url_parsed <- httr2::url_parse(url)
    path <- url_parsed$path
  } else {
    path <- url
  }

  # Remove leading slash if present
  path <- sub("^/+", "", path)

  xnat_download(path, dest_file, progress = progress, client = client)
  cli::cli_alert_success("Downloaded to {.file {dest_file}}")

  invisible(dest_file)
}

#' Download All Data for a Subject
#'
#' Downloads all experiments and scans for a subject.
#'
#' @param project_id The project identifier.
#' @param subject_id The subject identifier.
#' @param format Download format: "zip" (default) or "tar.gz".
#' @param dest_dir Destination directory.
#' @param progress Show download progress bar.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return Invisibly returns a character vector of downloaded file paths.
#'
#' @examples
#' \dontrun{
#' paths <- download_subject(
#'   project_id = "MyProject",
#'   subject_id = "Subject001",
#'   dest_dir = "~/Downloads"
#' )
#' }
#'
#' @export
download_subject <- function(project_id,
                              subject_id,
                              format = "zip",
                              dest_dir = getwd(),
                              progress = TRUE,
                              client = NULL) {
  check_string(project_id, "project_id")
  check_string(subject_id, "subject_id")

  # Create subject directory
  subject_dir <- file.path(dest_dir, subject_id)
  if (!dir.exists(subject_dir)) {
    dir.create(subject_dir, recursive = TRUE)
  }

  # Get experiments
  experiments <- list_experiments(project_id, subject_id, client = client)

  if (nrow(experiments) == 0) {
    cli::cli_alert_warning("No experiments found for subject {.val {subject_id}}")
    return(invisible(character(0)))
  }

  cli::cli_alert_info("Downloading {nrow(experiments)} experiment{?s} for subject {.val {subject_id}}")

  # Download each experiment
  paths <- character(nrow(experiments))
  for (i in seq_len(nrow(experiments))) {
    exp_id <- experiments$ID[i]
    paths[i] <- download_files(
      project_id = project_id,
      subject_id = subject_id,
      experiment_id = exp_id,
      format = format,
      dest_dir = subject_dir,
      progress = progress,
      client = client
    )
  }

  cli::cli_alert_success("Downloaded all data for subject {.val {subject_id}}")
  invisible(paths)
}

#' Download an Experiment Archive
#'
#' Downloads an experiment scan selection (`ALL` by default) as an archive.
#'
#' @param experiment_id Experiment identifier.
#' @param scan_id Scan ID/type to download; defaults to `"ALL"`.
#' @param format Archive format: `"zip"` (default) or `"tar.gz"`.
#' @param dest_dir Destination directory. Defaults to [tempdir()].
#' @param dest_file Optional destination filename. If `NULL`, auto-generated.
#' @param extract If `TRUE`, extract archive contents and return extracted paths.
#' @param progress Show download progress bar. Default `TRUE`.
#' @param strict If `TRUE` (default), raise errors on failed download. If `FALSE`,
#'   return `NULL` on failure.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return Archive path, extracted file paths (when `extract = TRUE`), or `NULL`
#'   on failure when `strict = FALSE`.
#' @export
download_experiment <- function(experiment_id,
                                scan_id = "ALL",
                                format = "zip",
                                dest_dir = tempdir(),
                                dest_file = NULL,
                                extract = FALSE,
                                progress = TRUE,
                                strict = TRUE,
                                client = NULL) {
  check_string(experiment_id, "experiment_id")
  check_string(scan_id, "scan_id")
  check_string(format, "format")

  if (!format %in% c("zip", "tar.gz")) {
    cli::cli_abort("{.arg format} must be one of {.val zip} or {.val tar.gz}.")
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(dest_file)) {
    ext <- if (identical(format, "zip")) ".zip" else ".tar.gz"
    dest_file <- paste0(experiment_id, ext)
  }
  dest_path <- file.path(dest_dir, dest_file)

  path <- xnat_path(
    "data/experiments", url_encode(experiment_id),
    "scans", url_encode(scan_id),
    "files"
  )

  download_impl <- function() {
    xnat_download(
      path = path,
      dest_file = dest_path,
      query = list(format = format),
      progress = isTRUE(progress),
      client = client
    )
  }

  if (isTRUE(strict)) {
    download_impl()
  } else {
    ok <- tryCatch({
      download_impl()
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning("Download failed for {.val {experiment_id}}: {conditionMessage(e)}")
      FALSE
    })
    if (!ok) {
      return(NULL)
    }
  }

  if (!isTRUE(extract)) {
    return(dest_path)
  }

  if (identical(format, "zip")) {
    extracted <- utils::unzip(dest_path, exdir = dest_dir)
    return(as_absolute_paths(extracted, base_dir = dest_dir))
  }

  extracted <- utils::untar(dest_path, exdir = dest_dir, list = TRUE)
  utils::untar(dest_path, exdir = dest_dir)
  as_absolute_paths(extracted, base_dir = dest_dir)
}

#' @noRd
as_absolute_paths <- function(paths, base_dir) {
  if (length(paths) == 0) {
    return(paths)
  }

  absolute <- grepl("^(/|[A-Za-z]:[/\\\\])", paths)
  paths[!absolute] <- file.path(base_dir, paths[!absolute])
  paths
}

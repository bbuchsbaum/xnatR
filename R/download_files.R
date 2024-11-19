
#' Download Files from XNAT
#'
#' Downloads files from the XNAT server using cached authentication credentials.
#'
#' @param project_id The ID of the project.
#' @param subject_id The ID of the subject.
#' @param experiment_id The ID of the experiment.
#' @param scan_id The ID of the scan (use "ALL" for all scans).
#' @param resource Optional resource name (e.g., "DICOM"). Use `NULL` to download all resources.
#' @param format The format of the downloaded files (default is "zip").
#' @param dest_dir The destination directory where the file will be saved (default is current working directory).
#'
#' @return Invisibly returns the response object if successful. Stops execution if the download fails.
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   download_files(
#'     project_id = "BuBr_M190_BA",
#'     subject_id = "BuBr_M190_BA_14972",
#'     experiment_id = "BuBr_M190_BA_14972_MRI_20240108",
#'     scan_id = "ALL",
#'     resource = "DICOM",
#'     format = "zip",
#'     dest_dir = "~/Downloads"
#'   )
#' }
#' @export
download_files <- function(
    project_id,
    subject_id,
    experiment_id,
    scan_id,
    resource = NULL,
    format = "zip",
    dest_dir = getwd()
) {
  # Check if authenticated
  if (is.null(xnatR_env$auth_header) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Please run `authenticate_xnat()` first.", call. = FALSE)
  }

  # Construct the API endpoint
  base_url <- xnatR_env$base_url
  if (!is.null(resource)) {
    # Specific resource download
    url <- sprintf(
      "%s/data/projects/%s/subjects/%s/experiments/%s/scans/%s/resources/%s/files?format=%s",
      base_url, project_id, subject_id, experiment_id, scan_id, resource, format
    )
  } else {
    # All resources download
    url <- sprintf(
      "%s/data/projects/%s/subjects/%s/experiments/%s/scans/%s/files?format=%s",
      base_url, project_id, subject_id, experiment_id, scan_id, format
    )
  }

  # Define the destination file name
  if (scan_id == "ALL") {
    scan_part <- "ALL"
  } else {
    scan_part <- scan_id
  }

  if (!is.null(resource)) {
    resource_part <- resource
  } else {
    resource_part <- "ALL"
  }

  dest_file <- file.path(dest_dir, sprintf("%s_%s_%s.zip", project_id, scan_part, resource_part))

  # Perform the GET request to download the file
  response <- httr::GET(
    url = url,
    httr::add_headers(Authorization = xnatR_env$auth_header),
    httr::config(ssl_verifypeer = FALSE),  # Disable SSL verification (use with caution)
    httr::write_disk(dest_file, overwrite = TRUE),
    httr::progress()
  )

  # Check if the download was successful
  if (httr::status_code(response) == 200) {
    message("File downloaded successfully to ", dest_file)
    invisible(response)
  } else {
    # Print detailed error information
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Failed to download files. Please check the parameters and your credentials.")
  }
}


#' Download All Subjects' Data from a Project
#'
#' Downloads zip archives for all subjects within a specified project from the XNAT server.
#'
#' @param project_id The ID of the project (e.g., "TEST").
#' @param format The format of the downloaded files (default is "zip").
#' @param dest_dir The destination directory where the files will be saved (default is current working directory).
#'
#' @return Invisibly returns a list of responses if successful. Stops execution if any download fails.
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   download_all_subjects(
#'     project_id = "TEST",
#'     format = "zip",
#'     dest_dir = "~/Downloads/TEST_Subjects"
#'   )
#' }
#' @export
download_all_subjects <- function(project_id, format = "zip", dest_dir = getwd()) {
  # Validate inputs
  if (missing(project_id) || !is.character(project_id)) {
    stop("Please provide a valid `project_id` as a character string.", call. = FALSE)
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    message("Created destination directory: ", dest_dir)
  }

  # List all subjects in the project
  subjects_df <- list_subjects(project_id = project_id)

  if (nrow(subjects_df) == 0) {
    stop("No subjects found in project ", project_id, ".", call. = FALSE)
  }

  # Initialize a list to store responses
  responses <- list()

  # Iterate over each subject and download their data
  for (i in seq_len(nrow(subjects_df))) {
    subject_id <- subjects_df$ID[i]
    subject_label <- subjects_df$label[i]

    # Construct the download URL
    # Assuming the URL to download all files for a subject is:
    # /data/projects/{PROJECT_ID}/subjects/{SUBJECT_ID}/files?format=zip
    download_url <- sprintf(
      "%s/data/projects/%s/subjects/%s/files?format=%s",
      xnatR_env$base_url,
      URLencode(project_id, reserved = TRUE),
      URLencode(subject_id, reserved = TRUE),
      URLencode(format, reserved = TRUE)
    )

    # Define the destination file name
    dest_file <- file.path(dest_dir, sprintf("%s_%s.%s", project_id, subject_id, format))

    message("Downloading subject ", subject_id, " (", subject_label, ")...")

    # Perform the GET request with preemptive authentication and SSL verification disabled
    response <- GET(
      url = download_url,
      add_headers(Authorization = xnatR_env$auth_header),
      config(ssl_verifypeer = FALSE),  # Disable SSL verification (use with caution)
      write_disk(dest_file, overwrite = TRUE),
      progress()
    )

    # Check if the download was successful
    if (status_code(response) == 200) {
      message("Successfully downloaded: ", dest_file)
      responses[[subject_id]] <- response
    } else {
      # Print detailed error information for debugging
      warning("Failed to download subject ", subject_id, ". Status code: ", status_code(response))
      print(content(response, as = "text", encoding = "UTF-8"))
      stop("Download failed for subject ", subject_id, ".")
    }
  }

  message("All subjects downloaded successfully to ", dest_dir)

  invisible(responses)
}

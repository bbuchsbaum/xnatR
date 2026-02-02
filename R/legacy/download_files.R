
# File: R/download_files.R

#' Download Files from XNAT
#'
#' Downloads files from the XNAT server using cached authentication credentials.
#'
#' @param project_id The ID of the project.
#' @param subject_id The ID of the subject.
#' @param experiment_id The ID of the experiment.
#' @param scan_id The ID of the scan (use "ALL" for all scans). Defaults to "ALL".
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
    scan_id = "ALL",
    resource = NULL,
    format = "zip",
    dest_dir = getwd()
) {
  # Check authentication
  if (is.null(xnatR_env$auth_header) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Please run `authenticate_xnat()` first.", call. = FALSE)
  }

  # Validate inputs
  if (missing(project_id) || !is.character(project_id)) {
    stop("Please provide a valid 'project_id' as a character string.", call. = FALSE)
  }
  if (missing(subject_id) || !is.character(subject_id)) {
    stop("Please provide a valid 'subject_id' as a character string.", call. = FALSE)
  }
  if (missing(experiment_id) || !is.character(experiment_id)) {
    stop("Please provide a valid 'experiment_id' as a character string.", call. = FALSE)
  }

  # Ensure destination directory exists
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    message("Created destination directory: ", dest_dir)
  }

  # Construct the API endpoint
  base_url <- xnatR_env$base_url

  # URL encode parameters
  project_id_enc <- utils::URLencode(project_id, reserved = TRUE)
  subject_id_enc <- utils::URLencode(subject_id, reserved = TRUE)
  experiment_id_enc <- utils::URLencode(experiment_id, reserved = TRUE)
  scan_id_enc <- utils::URLencode(scan_id, reserved = TRUE)
  format_enc <- utils::URLencode(format, reserved = TRUE)

  if (!is.null(resource)) {
    resource_enc <- utils::URLencode(resource, reserved = TRUE)
    # Specific resource download
    url <- sprintf(
      "%s/data/projects/%s/subjects/%s/experiments/%s/scans/%s/resources/%s/files?format=%s",
      base_url, project_id_enc, subject_id_enc, experiment_id_enc, scan_id_enc, resource_enc, format_enc
    )
  } else {
    # All resources download
    url <- sprintf(
      "%s/data/projects/%s/subjects/%s/experiments/%s/scans/%s/files?format=%s",
      base_url, project_id_enc, subject_id_enc, experiment_id_enc, scan_id_enc, format_enc
    )
  }

  # Define the destination file name
  dest_file <- file.path(dest_dir, sprintf("%s_%s_%s_%s_%s.zip",
                                           project_id,
                                           subject_id,
                                           experiment_id,
                                           scan_id,
                                           ifelse(is.null(resource), "ALL", resource)
  ))

  # Set SSL verification
  ssl_config <- if (xnatR_env$ssl_verify) {
    httr::config()
  } else {
    httr::config(ssl_verifypeer = FALSE)
  }

  # Perform the GET request to download the file
  response <- httr::GET(
    url = url,
    httr::add_headers(Authorization = xnatR_env$auth_header),
    ssl_config,
    httr::write_disk(dest_file, overwrite = TRUE),
    httr::progress()
  )

  # Check if the download was successful
  if (httr::status_code(response) == 200) {
    message("File downloaded successfully to ", dest_file)
    invisible(response)
  } else {
    # Print detailed error information
    warning("Failed to download files. Status code: ", httr::status_code(response))
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Failed to download files. Please check the parameters and your credentials.")
  }
}

# File: R/download_subject.R

#' Download Data for a Subject
#'
#' Downloads all experiments and scans for a given subject within a specified project from the XNAT server.
#'
#' @param project_id The ID of the project.
#' @param subject_id The ID of the subject.
#' @param format The format of the downloaded files (default is "zip").
#' @param dest_dir The destination directory where the files will be saved (default is current working directory).
#'
#' @return Invisibly returns a list of responses if successful. Stops execution if any download fails.
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   download_subject(
#'     project_id = "TEST",
#'     subject_id = "SUBJECT_ID",
#'     format = "zip",
#'     dest_dir = "~/Downloads/TEST_Subjects/SUBJECT_ID"
#'   )
#' }
#' @export
download_subject <- function(project_id, subject_id, format = "zip", dest_dir = getwd()) {
  # Validate inputs
  if (missing(project_id) || !is.character(project_id)) {
    stop("Please provide a valid 'project_id' as a character string.", call. = FALSE)
  }
  if (missing(subject_id) || !is.character(subject_id)) {
    stop("Please provide a valid 'subject_id' as a character string.", call. = FALSE)
  }

  # Ensure destination directory exists
  dest_dir <- file.path(dest_dir, subject_id)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    message("Created destination directory: ", dest_dir)
  }

  # List all experiments for the subject
  experiments_df <- list_experiments(project_id = project_id, subject_id = subject_id)

  if (nrow(experiments_df) == 0) {
    warning("No experiments found for subject ", subject_id, " in project ", project_id, ".")
    return(invisible(NULL))
  }

  # Initialize a list to store responses
  responses <- list()

  # Iterate over each experiment and download all scans
  for (i in seq_len(nrow(experiments_df))) {
    experiment_id <- experiments_df$ID[i]
    experiment_label <- experiments_df$label[i]

    message("Downloading data for experiment ", experiment_id, " (", experiment_label, ")...")

    # Call download_files function
    response <- download_files(
      project_id = project_id,
      subject_id = subject_id,
      experiment_id = experiment_id,
      scan_id = "ALL",
      resource = NULL,
      format = format,
      dest_dir = dest_dir
    )

    # Store the response
    responses[[experiment_id]] <- response
  }

  message("Downloaded data for subject ", subject_id, " to ", dest_dir)

  invisible(responses)
}



# File: R/download_all_subjects.R

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

  # Ensure destination directory exists
  dest_dir <- file.path(dest_dir, project_id)
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

    message("Downloading data for subject ", subject_id, " (", subject_label, ")...")

    # Call download_subject function
    response <- download_subject(
      project_id = project_id,
      subject_id = subject_id,
      format = format,
      dest_dir = dest_dir
    )

    # Store the response
    responses[[subject_id]] <- response
  }

  message("All subjects downloaded successfully to ", dest_dir)

  invisible(responses)
}

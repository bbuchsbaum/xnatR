# File: R/download.R

#' Download File from XNAT
#'
#' Downloads a file from the XNAT server using the authenticated session.
#'
#' @param url The full URL to the file resource you want to download.
#' @param dest_file The destination path where the downloaded file will be saved.
#'
#' @return Invisibly returns the response object if successful. Stops execution if the download fails.
#' @examples
#' \dontrun{
#' download_xnat_file(
#'   url = "https://your-xnat-server/data/projects/PROJECT_ID/subjects/SUBJECT_ID/experiments/EXPERIMENT_ID/scans/SCAN_ID/resources/RESOURCE_ID/files/FILE_NAME",
#'   dest_file = "downloaded_file.zip"
#' )
#' }
#' @export
download_xnat_file <- function(url, dest_file) {
  # Validate inputs
  if (missing(url) || missing(dest_file)) {
    stop("Both parameters 'url' and 'dest_file' must be provided.")
  }

  # Check authentication
  if (is.null(xnatR_env$auth_header)) {
    stop("Not authenticated. Please run `authenticate_xnat()` first.", call. = FALSE)
  }

  # Set SSL verification
  ssl_config <- if (xnatR_env$ssl_verify) {
    httr::config()
  } else {
    httr::config(ssl_verifypeer = FALSE)
  }

  # Perform the GET request with authentication, follow redirects
  response <- httr::GET(
    url = url,
    httr::add_headers(Authorization = xnatR_env$auth_header),
    ssl_config,
    httr::write_disk(dest_file, overwrite = TRUE),  # Write to specified file
    httr::progress()  # Show progress bar
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    message("File downloaded successfully to ", dest_file)
    invisible(response)
  } else {
    # Print detailed error information for debugging
    warning("Failed to download file. Status code: ", httr::status_code(response))
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Download failed. Please check the URL and your credentials.")
  }
}

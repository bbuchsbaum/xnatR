#' Authenticate with XNAT and Cache Credentials
#'
#' Authenticates with the XNAT server using credentials from the config file or provided directly.
#' Upon successful authentication, caches the credentials for use in other functions.
#'
#' @param base_url The base URL of the XNAT server. If `NULL`, loaded from config.
#' @param username The username for XNAT authentication. If `NULL`, loaded from config.
#' @param password The password for XNAT authentication. Overrides config if provided.
#' @param token An API token for XNAT authentication. Overrides config if provided.
#'
#' @return Invisibly returns `TRUE` if authentication is successful. Stops execution if it fails.
#' @export
authenticate_xnat <- function(base_url = NULL, username = NULL, password = NULL, token = NULL) {
  # If any parameter is NULL, attempt to load from config
  if (is.null(base_url) || is.null(username)) {
    creds <- try(load_credentials(), silent = TRUE)

    if (inherits(creds, "try-error")) {
      stop("Failed to load credentials from config. Please provide credentials or run `initialize_config()` and set your credentials.", call. = FALSE)
    }

    if (is.null(base_url)) base_url <- creds$base_url
    if (is.null(username)) username <- creds$username
    if (is.null(token) && is.null(password)) {
      password <- creds$password
      token <- creds$token
    }
  }

  if (is.null(password) && is.null(token)) {
    stop("Either password or token must be provided for authentication.")
  }

  # Construct the Authorization header for preemptive Basic Auth
  if (!is.null(password)) {
    # Basic Authentication
    auth_string <- paste0(username, ":", password)
    auth_encoded <- base64enc::base64encode(charToRaw(auth_string))
    auth_header <- paste("Basic", auth_encoded)
  } else {
    # Token-Based Authentication (assuming the server accepts tokens via Basic Auth)
    auth_string <- paste0(username, ":", token)
    auth_encoded <- base64enc::base64encode(charToRaw(auth_string))
    auth_header <- paste("Basic", auth_encoded)
  }

  # Correctly concatenate the base_url with the API path
  base_url <- sub("/+$", "", base_url)  # Remove trailing slash if any
  projects_url <- paste0(base_url, "/data/projects")

  # Perform the GET request with preemptive auth and SSL verification disabled
  response <- httr::GET(
    url = projects_url,
    httr::add_headers(Authorization = auth_header),
    httr::config(ssl_verifypeer = FALSE),  # Disable SSL verification (use with caution)
    httr::verbose()
  )

  # Check the response status
  if (httr::status_code(response) != 200) {
    # Print detailed error information for debugging
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Authentication failed. Please check your credentials.")
  }

  # Store credentials in the package environment
  xnatR_env$auth_header <- auth_header
  xnatR_env$base_url <- base_url

  message("Authentication successful.")

  invisible(TRUE)
}



#' Download Files from XNAT
#'
#' Downloads files from the XNAT server using basic authentication, following redirects,
#' and disabling SSL verification to match the behavior of a successful `wget` command.
#'
#' @param url The full URL to the file resource you want to download.
#' @param username Your XNAT username.
#' @param password Your XNAT password.
#' @param dest_file The destination path where the downloaded file will be saved.
#'
#' @return Invisibly returns the response object if successful. Stops execution if the download fails.
#' @examples
#' \dontrun{
#' download_xnat_files(
#'   url = "https://rrinid.rotman-baycrest.on.ca/spred/data/projects/BuBr_M190_BA/subjects/BuBr_M190_BA_14972/experiments/BuBr_M190_BA_14972_MRI_20240108/scans/ALL/resources/DICOM/files?format=zip",
#'   username = "buchsbaum",
#'  pa
#'   dest_file = "BuBr_M190_BA_MRI_14972__20240108_Scan_ALL.zip"
#' )
#' }
#' @export
download_xnat_files <- function(url, username, password, dest_file) {
  # Validate inputs
  if (missing(url) || missing(username) || missing(password) || missing(dest_file)) {
    stop("All parameters (url, username, password, dest_file) must be provided.")
  }

  # Perform the GET request with basic authentication, disable SSL verification, follow redirects
  response <- GET(
    url = url,
    authenticate(username, password, type = "basic"),
    config(ssl_verifypeer = FALSE),  # Disable SSL verification
    write_disk(dest_file, overwrite = TRUE),  # Write to specified file
    progress()  # Show progress bar
  )

  # Check if the request was successful
  if (status_code(response) == 200) {
    message("File downloaded successfully to ", dest_file)
    invisible(response)
  } else {
    # Print detailed error information for debugging
    warning("Failed to download file. Status code: ", status_code(response))
    print(content(response, as = "text", encoding = "UTF-8"))
    stop("Download failed. Please check the URL and your credentials.")
  }
}



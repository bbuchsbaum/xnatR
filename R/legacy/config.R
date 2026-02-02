# File: R/config.R

#' Initialize Configuration File for xnatR
#'
#' Creates a configuration file template for storing XNAT credentials.
#' The file is saved in the user's home directory as '.xnatR_config.yml'.
#'
#' @return Invisibly returns TRUE if the file is created successfully.
#' @export
initialize_config <- function() {
  config_path <- file.path("~", ".xnatR_config.yml")
  if (file.exists(config_path)) {
    message("Configuration file already exists at ", config_path)
    return(invisible(TRUE))
  }

  # Create a template
  config_template <- "
# xnatR Configuration File

base_url: 'https://your-xnat-server-url'

# Authentication using username and password
username: 'your-username'
password: 'your-password'

# Or authentication using API token
# token: 'your-api-token'

# SSL Verification (TRUE or FALSE)
ssl_verify: TRUE
"

  writeLines(config_template, con = config_path)
  message("Configuration file created at ", config_path)
  message("Please edit the file and enter your XNAT credentials.")
  invisible(TRUE)
}

#' Load Credentials from Configuration File
#'
#' Loads XNAT credentials from the configuration file located at '~/.xnatR_config.yml'.
#'
#' @return A list containing base_url, username, password, token, and ssl_verify.
#' @export
load_credentials <- function() {
  config_path <- file.path("~", ".xnatR_config.yml")
  if (!file.exists(config_path)) {
    stop("Configuration file not found. Please run `initialize_config()` to create one.")
  }

  creds <- yaml::yaml.load_file(config_path)
  if (is.null(creds$base_url) || is.null(creds$username)) {
    stop("Configuration file is missing required fields 'base_url' and 'username'.")
  }

  # Return credentials
  list(
    base_url = creds$base_url,
    username = creds$username,
    password = creds$password,
    token = creds$token,
    ssl_verify = ifelse(is.null(creds$ssl_verify), TRUE, creds$ssl_verify)
  )
}

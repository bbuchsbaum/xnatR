#' Authenticate with an XNAT Server
#'
#' Establishes authentication credentials for communicating with an XNAT server.
#' Credentials are stored in the package environment for use by other functions.
#'
#' @param base_url The base URL of the XNAT server (e.g., "https://central.xnat.org").
#'   If NULL, checks XNATR_HOST environment variable, then config file.
#' @param username XNAT username. If NULL, checks XNATR_USER environment variable,
#'   then config file.
#' @param password XNAT password. If NULL, checks XNATR_PASS environment variable,
#'   then config file.
#' @param ssl_verify Whether to verify SSL certificates. Default TRUE.
#' @param verify Whether to verify credentials by making a test request. Default TRUE.
#'
#' @return Invisibly returns TRUE if authentication is successful.
#'
#' @examples
#' \dontrun{
#' # Direct credentials
#' authenticate_xnat(
#'   base_url = "https://central.xnat.org",
#'   username = "guest",
#'   password = "guest"
#' )
#'
#' # Using environment variables (XNATR_HOST, XNATR_USER, XNATR_PASS)
#' authenticate_xnat()
#'
#' # Using config file (~/.xnatR_config.yml)
#' authenticate_xnat()
#' }
#'
#' @export
authenticate_xnat <- function(base_url = NULL,
                               username = NULL,
                               password = NULL,
                               ssl_verify = TRUE,
                               verify = TRUE) {
  # Resolve credentials from multiple sources
  creds <- resolve_credentials(base_url, username, password)

  # Validate we have all required credentials
  if (is.null(creds$base_url) || !nzchar(creds$base_url)) {
    cli::cli_abort(c(
      "No XNAT server URL provided.",
      "i" = "Provide {.arg base_url}, set {.envvar XNATR_HOST}, or configure {.file ~/.xnatR_config.yml}."
    ))
  }
  if (is.null(creds$username) || !nzchar(creds$username)) {
    cli::cli_abort(c(
      "No username provided.",
      "i" = "Provide {.arg username}, set {.envvar XNATR_USER}, or configure {.file ~/.xnatR_config.yml}."
    ))
  }
  if (is.null(creds$password) || !nzchar(creds$password)) {
    cli::cli_abort(c(
      "No password provided.",
      "i" = "Provide {.arg password}, set {.envvar XNATR_PASS}, or configure {.file ~/.xnatR_config.yml}."
    ))
  }

  # Store credentials in package environment
  xnatR_env$base_url <- sub("/+$", "", creds$base_url)  # Remove trailing slash

  xnatR_env$username <- creds$username
  xnatR_env$password <- creds$password
  xnatR_env$ssl_verify <- ssl_verify

  # Verify credentials if requested
 if (verify) {
    tryCatch({
      # Test with a simple request to list projects
      xnat_get("data/projects")
      cli::cli_alert_success("Authenticated to {.url {xnatR_env$base_url}}")
    }, error = function(e) {
      # Clear credentials on failure
      xnatR_env$base_url <- NULL
      xnatR_env$username <- NULL
      xnatR_env$password <- NULL

      if (inherits(e, "httr2_http_401") || inherits(e, "httr2_http_403")) {
        cli::cli_abort(c(
          "Authentication failed.",
          "x" = "Invalid username or password."
        ))
      } else {
        cli::cli_abort(c(
          "Failed to connect to XNAT server.",
          "x" = conditionMessage(e)
        ))
      }
    })
  }

  invisible(TRUE)
}

#' Resolve credentials from multiple sources
#'
#' Priority: explicit args > environment variables > config file
#'
#' @param base_url Explicit base_url or NULL
#' @param username Explicit username or NULL
#' @param password Explicit password or NULL
#' @return List with base_url, username, password
#' @noRd
resolve_credentials <- function(base_url = NULL, username = NULL, password = NULL) {
  # Start with explicit arguments
  creds <- list(
    base_url = base_url,
    username = username,
    password = password
  )

  # Fill from environment variables
  if (is.null(creds$base_url)) {
    env_host <- Sys.getenv("XNATR_HOST", unset = "")
    if (nzchar(env_host)) creds$base_url <- env_host
  }
  if (is.null(creds$username)) {
    env_user <- Sys.getenv("XNATR_USER", unset = "")
    if (nzchar(env_user)) creds$username <- env_user
  }
  if (is.null(creds$password)) {
    env_pass <- Sys.getenv("XNATR_PASS", unset = "")
    if (nzchar(env_pass)) creds$password <- env_pass
  }

  # Fill from config file if still missing
  if (is.null(creds$base_url) || is.null(creds$username) || is.null(creds$password)) {
    config <- try(load_config(), silent = TRUE)
    if (!inherits(config, "try-error")) {
      if (is.null(creds$base_url)) creds$base_url <- config$base_url
      if (is.null(creds$username)) creds$username <- config$username
      if (is.null(creds$password)) creds$password <- config$password
    }
  }

  creds
}

#' Load credentials from config file
#' @return List with credentials or error
#' @noRd
load_config <- function() {
  config_path <- path.expand("~/.xnatR_config.yml")

  if (!file.exists(config_path)) {
    stop("Config file not found")
  }

  yaml::yaml.load_file(config_path)
}

#' Initialize Configuration File
#'
#' Creates a template configuration file at ~/.xnatR_config.yml
#'
#' @return Invisibly returns the path to the config file.
#'
#' @examples
#' \dontrun{
#' initialize_config()
#' # Edit ~/.xnatR_config.yml with your credentials
#' }
#'
#' @export
initialize_config <- function() {
  config_path <- path.expand("~/.xnatR_config.yml")

  if (file.exists(config_path)) {
    cli::cli_alert_info("Config file already exists at {.file {config_path}}")
    return(invisible(config_path))
  }

  template <- "# xnatR Configuration File
#
# Authentication credentials for XNAT server
base_url: 'https://your-xnat-server.org'
username: 'your-username'
password: 'your-password'

# SSL verification (set to false for self-signed certificates)
ssl_verify: true
"

  writeLines(template, config_path)
  cli::cli_alert_success("Created config file at {.file {config_path}}")
  cli::cli_alert_info("Edit the file to add your XNAT credentials.")

  invisible(config_path)
}

#' Log out from XNAT
#'
#' Clears stored credentials and optionally invalidates the server session.
#'
#' @param invalidate_session Whether to send a logout request to the server.
#'   Default FALSE (just clears local credentials).
#'
#' @return Invisibly returns TRUE.
#'
#' @examples
#' \dontrun{
#' xnat_logout()
#' }
#'
#' @export
xnat_logout <- function(invalidate_session = FALSE) {
  if (invalidate_session && !is.null(xnatR_env$jsession)) {
    tryCatch({
      xnat_delete("data/JSESSION")
    }, error = function(e) {
      cli::cli_alert_warning("Failed to invalidate server session: {conditionMessage(e)}")
    })
  }

  # Clear all credentials
  xnatR_env$base_url <- NULL
  xnatR_env$username <- NULL
  xnatR_env$password <- NULL
  xnatR_env$jsession <- NULL

  cli::cli_alert_success("Logged out.")
  invisible(TRUE)
}

#' Check if authenticated
#'
#' @return TRUE if credentials are stored, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' if (is_authenticated()) {
#'   list_projects()
#' }
#' }
#'
#' @export
is_authenticated <- function() {
  !is.null(xnatR_env$base_url) &&
    !is.null(xnatR_env$username) &&
    !is.null(xnatR_env$password)
}

#' Get current XNAT server URL
#'
#' @return The current server URL or NULL if not authenticated.
#'
#' @examples
#' \dontrun{
#' xnat_server()
#' }
#'
#' @export
xnat_server <- function() {
  xnatR_env$base_url
}

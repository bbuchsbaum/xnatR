#' Authenticate with an XNAT Server
#'
#' Establishes authentication credentials for communicating with an XNAT server.
#' Credentials are stored in the package environment for use by other functions.
#'
#' @param base_url The base URL of the XNAT server (e.g., "https://central.xnat.org").
#'   If NULL, checks XNATR_HOST environment variable, then config file, then .netrc.
#' @param username XNAT username. If NULL, checks XNATR_USER environment variable,
#'   then config file, then .netrc. For XNAT 1.8+ with auth providers, use
#'   "provider/username" format.
#' @param password XNAT password. If NULL, checks XNATR_PASS environment variable,
#'   then config file, then .netrc.
#' @param ssl_verify Whether to verify SSL certificates. Default TRUE.
#' @param verify Whether to verify credentials by making a test request. Default TRUE.
#' @param use_jsession Whether to establish a JSESSION cookie-based session instead
#'   of using Basic Auth for each request. Can improve performance. Default FALSE.
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
#'
#' # Using .netrc file
#' authenticate_xnat(base_url = "https://central.xnat.org")
#'
#' # With JSESSION for better performance
#' authenticate_xnat(
#'   base_url = "https://central.xnat.org",
#'   username = "myuser",
#'   password = "mypass",
#'   use_jsession = TRUE
#' )
#'
#' # XNAT 1.8+ with auth provider
#' authenticate_xnat(
#'   base_url = "https://myxnat.org",
#'   username = "ldap/myuser",
#'   password = "mypass"
#' )
#' }
#'
#' @export
authenticate_xnat <- function(base_url = NULL,
                               username = NULL,
                               password = NULL,
                               ssl_verify = TRUE,
                               verify = TRUE,
                               use_jsession = FALSE) {
  # Resolve credentials from multiple sources
  creds <- resolve_credentials(base_url, username, password)

  # Validate we have all required credentials
  if (is.null(creds$base_url) || !nzchar(creds$base_url)) {
    cli::cli_abort(c(
      "No XNAT server URL provided.",
      "i" = "Provide {.arg base_url}, set {.envvar XNATR_HOST}, configure {.file ~/.xnatR_config.yml}, or add to {.file ~/.netrc}."
    ))
  }
  if (is.null(creds$username) || !nzchar(creds$username)) {
    cli::cli_abort(c(
      "No username provided.",
      "i" = "Provide {.arg username}, set {.envvar XNATR_USER}, configure {.file ~/.xnatR_config.yml}, or add to {.file ~/.netrc}."
    ))
  }
  if (is.null(creds$password) || !nzchar(creds$password)) {
    cli::cli_abort(c(
      "No password provided.",
      "i" = "Provide {.arg password}, set {.envvar XNATR_PASS}, configure {.file ~/.xnatR_config.yml}, or add to {.file ~/.netrc}."
    ))
  }

  # Store credentials in package environment
  xnatR_env$base_url <- sub("/+$", "", creds$base_url)  # Remove trailing slash
  xnatR_env$username <- creds$username
  xnatR_env$password <- creds$password
  xnatR_env$ssl_verify <- ssl_verify
  xnatR_env$jsession <- NULL

  # Establish JSESSION if requested
  if (use_jsession) {
    tryCatch({
      jsession <- establish_jsession(
        base_url = xnatR_env$base_url,
        username = xnatR_env$username,
        password = xnatR_env$password,
        ssl_verify = xnatR_env$ssl_verify
      )
      xnatR_env$jsession <- jsession
      cli::cli_alert_info("Using JSESSION-based authentication")
    }, error = function(e) {
      cli::cli_alert_warning("Failed to establish JSESSION, falling back to Basic Auth: {conditionMessage(e)}")
    })
  }

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
      xnatR_env$jsession <- NULL

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

#' Establish a JSESSION cookie session
#'
#' Creates a server-side session and returns the JSESSION ID.
#'
#' @param base_url XNAT base URL
#' @param username XNAT username
#' @param password XNAT password
#' @param ssl_verify Whether to verify SSL certificates
#' @return JSESSION ID string
#' @noRd
establish_jsession <- function(base_url, username, password, ssl_verify = TRUE) {
  req <- httr2::request(sub("/+$", "", base_url)) |>
    httr2::req_url_path_append("data/JSESSION") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_method("POST")

  if (!isTRUE(ssl_verify)) {
    req <- httr2::req_options(req, ssl_verifypeer = FALSE)
  }

  resp <- httr2::req_perform(req)
  httr2::resp_body_string(resp)
}

#' Resolve credentials from multiple sources
#'
#' Priority: explicit args > environment variables > config file > .netrc
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

  # Fill from .netrc if still missing and we have a base_url
  if (!is.null(creds$base_url) && (is.null(creds$username) || is.null(creds$password))) {
    netrc_creds <- try(load_netrc(creds$base_url), silent = TRUE)
    if (!inherits(netrc_creds, "try-error")) {
      if (is.null(creds$username)) creds$username <- netrc_creds$username
      if (is.null(creds$password)) creds$password <- netrc_creds$password
    }
  }

  creds
}

#' Load credentials from .netrc file
#'
#' Parses ~/.netrc for credentials matching the given host.
#'
#' @param base_url The XNAT server URL to match
#' @return List with username and password, or error
#' @noRd
load_netrc <- function(base_url) {
  netrc_path <- path.expand("~/.netrc")

  if (!file.exists(netrc_path)) {
    stop(".netrc file not found")
  }

  # Extract hostname from URL
  host <- sub("^https?://", "", base_url)
  host <- sub("/.*$", "", host)
  host <- sub(":.*$", "", host)  # Remove port if present

  # Parse .netrc file
  lines <- readLines(netrc_path, warn = FALSE)
  content <- paste(lines, collapse = " ")

  # Simple .netrc parser - looks for machine/login/password
 pattern <- paste0("machine\\s+", gsub("\\.", "\\\\.", host),
                    "\\s+login\\s+(\\S+)\\s+password\\s+(\\S+)")
  match <- regmatches(content, regexec(pattern, content, ignore.case = TRUE))[[1]]

  if (length(match) < 3) {
    stop("No matching entry in .netrc for host: ", host)
  }

  list(username = match[2], password = match[3])
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

# For XNAT 1.8+ with auth providers, use provider/username format:
# username: 'ldap/your-username'

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
#'
#' # Also invalidate server session
#' xnat_logout(invalidate_session = TRUE)
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
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
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
is_authenticated <- function(client = NULL) {
  if (!is.null(client)) {
    if (!inherits(client, "xnat_client")) {
      cli::cli_abort("{.arg client} must be an xnat_client.")
    }
    return(!is.null(client$base_url) &&
      (!is.null(client$jsession) || (!is.null(client$username) && !is.null(client$password))))
  }

  !is.null(xnatR_env$base_url) &&
    (!is.null(xnatR_env$jsession) ||
      (!is.null(xnatR_env$username) && !is.null(xnatR_env$password)))
}

#' Get current XNAT server URL
#'
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return The current server URL or NULL if not authenticated.
#'
#' @examples
#' \dontrun{
#' xnat_server()
#' }
#'
#' @export
xnat_server <- function(client = NULL) {
  if (!is.null(client)) {
    if (!inherits(client, "xnat_client")) {
      cli::cli_abort("{.arg client} must be an xnat_client.")
    }
    return(client$base_url)
  }
  xnatR_env$base_url
}

#' Get current username
#'
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return The current username or NULL if not authenticated.
#'
#' @examples
#' \dontrun{
#' xnat_username()
#' }
#'
#' @export
xnat_username <- function(client = NULL) {
  if (!is.null(client)) {
    if (!inherits(client, "xnat_client")) {
      cli::cli_abort("{.arg client} must be an xnat_client.")
    }
    return(client$username)
  }
  xnatR_env$username
}

# -----------------------------------------------------------------------------
# Alias Token API
# -----------------------------------------------------------------------------

#' Issue an Alias Token
#'
#' Creates a new alias token for the authenticated user. Alias tokens can be
#' used instead of passwords for authentication and can be invalidated
#' independently.
#'
#' @return A list containing the token details:
#'   - `alias`: The alias (username-like identifier)
#'   - `secret`: The secret (password-like value)
#'   - `estimatedExpirationTime`: When the token expires
#'
#' @examples
#' \dontrun{
#' authenticate_xnat(base_url = "https://central.xnat.org",
#'                   username = "myuser", password = "mypass")
#'
#' token <- xnat_token_issue()
#' # Use token$alias and token$secret for subsequent authentication
#' }
#'
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @export
xnat_token_issue <- function(client = NULL) {
  result <- xnat_get("data/services/tokens/issue", client = client)

  # Flatten the result if nested
  if (!is.null(result$alias)) {
    token <- list(
      alias = result$alias,
      secret = result$secret,
      estimatedExpirationTime = result$estimatedExpirationTime
    )
  } else {
    token <- result
  }

  cli::cli_alert_success("Issued alias token: {.val {token$alias}}")
  token
}

#' Validate an Alias Token
#'
#' Checks if an alias token is still valid.
#'
#' @param alias The alias portion of the token.
#' @param secret The secret portion of the token.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A list with validation details:
#'   - `valid`: Logical, whether the token is valid
#'   - `alias`: The alias
#'   - `estimatedExpirationTime`: When the token expires (if valid
#'
#' @examples
#' \dontrun{
#' # Check if a token is still valid
#' result <- xnat_token_validate(alias = "my_alias", secret = "my_secret")
#' if (result$valid) {
#'   message("Token is valid until ", result$estimatedExpirationTime)
#' }
#' }
#'
#' @export
xnat_token_validate <- function(alias, secret, client = NULL) {
  check_string(alias, "alias")
  check_string(secret, "secret")

  path <- xnat_path("data/services/tokens/validate", alias, secret)

  tryCatch({
    result <- xnat_get(path, client = client)
    result$valid <- TRUE
    result
  }, error = function(e) {
    list(valid = FALSE, alias = alias, message = conditionMessage(e))
  })
}

#' Invalidate an Alias Token
#'
#' Revokes an alias token so it can no longer be used for authentication.
#'
#' @param alias The alias portion of the token to invalidate.
#' @param secret The secret portion of the token to invalidate.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return Invisibly returns TRUE if successful.
#'
#' @examples
#' \dontrun{
#' # Invalidate a token
#' xnat_token_invalidate(alias = "my_alias", secret = "my_secret")
#' }
#'
#' @export
xnat_token_invalidate <- function(alias, secret, client = NULL) {
  check_string(alias, "alias")
  check_string(secret, "secret")

  path <- xnat_path("data/services/tokens/invalidate", alias, secret)

  tryCatch({
    xnat_post(path, client = client)
    cli::cli_alert_success("Invalidated alias token: {.val {alias}}")
    invisible(TRUE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to invalidate token.",
      "x" = conditionMessage(e)
    ))
  })
}

#' List User's Alias Tokens
#'
#' Retrieves all active alias tokens for the authenticated user.
#'
#' @return A tibble of active tokens with columns:
#'   - `alias`: Token alias
#'   - `xdatUserId`: Associated user ID
#'   - `estimatedExpirationTime`: Expiration timestamp
#'
#' @examples
#' \dontrun{
#' tokens <- xnat_token_list()
#' print(tokens)
#' }
#'
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @export
xnat_token_list <- function(client = NULL) {
  result <- xnat_get("data/services/tokens/list", client = client)

  if (length(result) == 0) {
    return(tibble::tibble(
      alias = character(),
      xdatUserId = character(),
      estimatedExpirationTime = character()
    ))
  }

  tibble::as_tibble(result)
}

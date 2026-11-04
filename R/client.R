#' XNAT client objects
#'
#' `xnat_connect()` creates an explicit client object that can be passed to all
#' xnatR functions via the `client` argument. This avoids relying on global
#' session state stored in the package environment.
#'
#' `authenticate_xnat()` remains available for a global-session workflow.
#'
#' @param base_url Base URL of the XNAT server (e.g., `"https://central.xnat.org"`).
#' @param username Username, or `NULL` if using `jsession` only.
#' @param password Password, or `NULL` if using `jsession` only.
#' @param ssl_verify Whether to verify SSL certificates. Default `TRUE`.
#' @param verify Whether to verify credentials by making a test request. Default `TRUE`.
#' @param use_jsession Whether to establish a JSESSION cookie-based session instead
#'   of using Basic Auth for each request. Default `FALSE`.
#' @param jsession Optional existing JSESSIONID value. When provided, requests use
#'   cookie auth and do not send Basic Auth.
#' @param xnat_name Optional server nickname used for compatibility with Rxnat-style
#'   environment variables (`<XNAT_NAME>_RXNAT_USER`, `<XNAT_NAME>_RXNAT_PASS`).
#'
#' @param backend Transport backend: `httr2` (default) or system `curl`.
#' @param credential_callback Function returning a username/password list, or NULL
#'   to cancel. Diagnoses connectivity before calling it. Uses JSESSION, discards
#'   the supplied password, and calls again once on an expired read request.
#' @return `xnat_connect()` returns an `xnat_client`.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- xnat_connect(
#'   base_url = "https://central.xnat.org",
#'   username = "guest",
#'   password = "guest",
#'   use_jsession = TRUE
#' )
#'
#' projects <- list_projects(client = client)
#' }
xnat_connect <- function(base_url = NULL,
                         username = NULL,
                         password = NULL,
                         ssl_verify = TRUE,
                         verify = TRUE,
                         use_jsession = FALSE,
                         jsession = NULL,
                         xnat_name = NULL,
                         backend = c("httr2", "curl"),
                         credential_callback = NULL) {
  backend <- match.arg(backend)
  if (!is.null(xnat_name)) {
    check_string(xnat_name, "xnat_name")
    prefix <- toupper(xnat_name)
    user_var <- paste0(prefix, "_RXNAT_USER")
    pass_var <- paste0(prefix, "_RXNAT_PASS")

    if (is.null(username)) {
      env_user <- Sys.getenv(user_var, unset = "")
      if (nzchar(env_user)) {
        username <- env_user
      }
    } else {
      do.call(Sys.setenv, stats::setNames(list(username), user_var))
    }

    if (is.null(password)) {
      env_pass <- Sys.getenv(pass_var, unset = "")
      if (nzchar(env_pass)) {
        password <- env_pass
      }
    } else {
      do.call(Sys.setenv, stats::setNames(list(password), pass_var))
    }
  }

  creds <- resolve_credentials(base_url, username, password)

  if (is.null(creds$base_url) || !nzchar(creds$base_url)) {
    cli::cli_abort(c(
      "No XNAT server URL provided.",
      "i" = "Provide {.arg base_url}, set {.envvar XNATR_HOST}, configure {.file ~/.xnatR_config.yml}, or add to {.file ~/.netrc}."
    ))
  }

  client <- xnat_client(
    base_url = creds$base_url,
    username = creds$username,
    password = creds$password,
    ssl_verify = ssl_verify,
    jsession = jsession,
    backend = backend
  )

  if (!is.null(credential_callback)) {
    if (!is.function(credential_callback)) rlang::abort("credential_callback must be a function.")
    diagnostic <- xnat_diagnose(client$base_url, backend)
    if (!diagnostic$reachable) transport_abort(diagnostic$category)
    client$credential_callback <- credential_callback
    if (is.null(client$jsession)) {
      supplied <- request_credentials(credential_callback)
      client$username <- supplied$username
      client$password <- supplied$password
    }
    use_jsession <- TRUE
  }

  if (use_jsession && is.null(client$jsession)) {
    if (is.null(client$username) || is.null(client$password)) {
      client$jsession <- establish_anonymous_jsession(
        base_url = client$base_url,
        ssl_verify = client$ssl_verify,
        backend = backend
      )
    } else {
      client$jsession <- establish_jsession(
        base_url = client$base_url,
        username = client$username,
        password = client$password,
        ssl_verify = client$ssl_verify,
        backend = backend
      )
    }
  }

  client$session$jsession <- client$jsession
  if (!is.null(credential_callback)) client$password <- NULL

  if (verify) {
    xnat_get("data/projects", client = client)
  }

  client
}

#' Create an `xnat_client`
#'
#' Low-level constructor for client objects. Prefer `xnat_connect()` for most
#' use cases.
#'
#' @param base_url Base URL of the XNAT server.
#' @param username Username.
#' @param password Password.
#' @param ssl_verify Whether to verify SSL certificates. Default `TRUE`.
#' @param jsession Optional JSESSIONID value.
#'
#' @param backend Transport backend: `httr2` or system `curl`.
#' @return An `xnat_client`.
#' @export
xnat_client <- function(base_url,
                        username = NULL,
                        password = NULL,
                        ssl_verify = TRUE,
                        jsession = NULL, backend = c("httr2", "curl")) {
  backend <- match.arg(backend)
  validate_server_url(base_url)
  base_url <- sub("/+$", "", base_url)

  if (!is.null(username) && !is.null(password)) {
    check_string(username, "username")
    check_string(password, "password")
  }

  session <- new.env(parent = emptyenv())
  session$jsession <- jsession
  structure(
    list(
      base_url = base_url,
      username = username,
      password = password,
      ssl_verify = isTRUE(ssl_verify),
      jsession = jsession,
      backend = backend,
      session = session
    ),
    class = "xnat_client"
  )
}

#' @export
print.xnat_client <- function(x, ...) {
  cli::cli_h1("XNAT Client")
  cli::cli_text("Server: {.url {x$base_url}}")
  if (!is.null(x$username)) {
    cli::cli_text("User: {.val {x$username}}")
  }
  auth <- if (!is.null(x$jsession) && nzchar(x$jsession)) "JSESSION" else "Basic"
  cli::cli_text("Auth: {.val {auth}}")
  cli::cli_text("SSL verify: {.val {isTRUE(x$ssl_verify)}}")
  invisible(x)
}

#' Get the current global-session client
#'
#' Returns an `xnat_client` populated from the package environment (set by
#' `authenticate_xnat()`), or `NULL` if not authenticated.
#'
#' @return An `xnat_client` or `NULL`.
#' @export
xnat_current_client <- function() {
  if (is.null(xnatR_env$base_url)) return(NULL)
  client <- xnat_client(
    base_url = xnatR_env$base_url,
    username = xnatR_env$username,
    password = xnatR_env$password,
    ssl_verify = xnatR_env$ssl_verify,
    jsession = xnatR_env$session$jsession %||% xnatR_env$jsession,
    backend = xnatR_env$backend %||% "httr2"
  )
  client$credential_callback <- xnatR_env$credential_callback
  client$session <- xnatR_env$session %||% client$session
  client
}

#' Temporarily use a client as the global session
#'
#' This helper makes existing code written for the global-session workflow work
#' with an explicit `xnat_client`.
#'
#' @param client An `xnat_client`.
#' @param expr Expression to evaluate.
#'
#' @return The value of `expr`.
#' @export
with_xnat_client <- function(client, expr) {
  if (!inherits(client, "xnat_client")) {
    cli::cli_abort("{.arg client} must be an xnat_client.")
  }

  old <- xnat_current_client()
  extra <- lapply(c("backend", "session", "credential_callback"), function(k) xnatR_env[[k]])
  names(extra) <- c("backend", "session", "credential_callback")
  on.exit(for (k in names(extra)) xnatR_env[[k]] <- extra[[k]], add = TRUE)
  for (k in names(extra)) xnatR_env[[k]] <- client[[k]]
  on.exit({
    if (is.null(old)) {
      xnatR_env$base_url <- NULL
      xnatR_env$username <- NULL
      xnatR_env$password <- NULL
      xnatR_env$ssl_verify <- TRUE
      xnatR_env$jsession <- NULL
    } else {
      xnatR_env$base_url <- old$base_url
      xnatR_env$username <- old$username
      xnatR_env$password <- old$password
      xnatR_env$ssl_verify <- old$ssl_verify
      xnatR_env$jsession <- old$jsession
    }
  }, add = TRUE)

  xnatR_env$base_url <- client$base_url
  xnatR_env$username <- client$username
  xnatR_env$password <- client$password
  xnatR_env$ssl_verify <- client$ssl_verify
  xnatR_env$jsession <- client$jsession

  force(expr)
}

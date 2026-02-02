# Package environment for storing session state
xnatR_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
 # Initialize package environment with NULL values
  xnatR_env$base_url <- NULL
  xnatR_env$username <- NULL
  xnatR_env$password <- NULL
  xnatR_env$ssl_verify <- TRUE
  xnatR_env$jsession <- NULL

  # Check for environment variables
  env_host <- Sys.getenv("XNATR_HOST", unset = "")
  env_user <- Sys.getenv("XNATR_USER", unset = "")
  env_pass <- Sys.getenv("XNATR_PASS", unset = "")

  if (nzchar(env_host) && nzchar(env_user) && nzchar(env_pass)) {
    xnatR_env$base_url <- sub("/+$", "", env_host)
    xnatR_env$username <- env_user
    xnatR_env$password <- env_pass
  }
}

#' Get package option with fallback
#' @noRd
xnat_option <- function(name, default = NULL) {
  getOption(paste0("xnatR.", name), default = default)
}

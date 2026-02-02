# Setup file for testthat
# This file is run before tests

# Load httptest2 for mocking HTTP requests
library(httptest2)

# Get reference to package environment
get_xnatR_env <- function() {
  get("xnatR_env", envir = asNamespace("xnatR"))
}

# Helper to set up test authentication state
setup_test_auth <- function(base_url = "https://test.xnat.org") {
  env <- get_xnatR_env()
  env$base_url <- base_url
  env$username <- "test_user"
  env$password <- "test_pass"
  env$ssl_verify <- TRUE
}

# Helper to clear authentication state
clear_test_auth <- function() {
  env <- get_xnatR_env()
  env$base_url <- NULL
  env$username <- NULL
  env$password <- NULL
  env$ssl_verify <- TRUE
}

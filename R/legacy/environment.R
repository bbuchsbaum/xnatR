# File: R/environment.R

# Create a private environment to store package-level variables
xnatR_env <- new.env(parent = emptyenv())

# Initialize with NULL
xnatR_env$auth_header <- NULL
xnatR_env$base_url <- NULL
xnatR_env$ssl_verify <- TRUE

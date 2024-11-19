# File: R/environment.R

# Create a private environment to store package-level variables
xnatR_env <- new.env(parent = emptyenv())

# Initialize with NULL
xnatR_env$auth <- NULL

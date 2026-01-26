# Coding Conventions

**Analysis Date:** 2026-01-26

## Naming Patterns

**Files:**
- Snake_case with `.R` extension: `authenticate.R`, `list_projects.R`, `download_files.R`
- Single responsibility per file (one or more related functions)
- Prefixes indicate grouping: `list_*.R` for retrieval functions, `download_*.R` for file operations
- Example: `R/list_projects.R`, `R/download_files.R`, `R/authenticate.R`

**Functions:**
- Snake_case (lowercase with underscores): `authenticate_xnat()`, `list_projects()`, `download_files()`
- Verb-noun pattern for clarity: `list_subjects()`, `search_projects()`, `download_xnat_file()`
- Exported functions are prefixed descriptively: `authenticate_xnat`, `download_xnat_file`, `download_xnat_files`
- Examples from codebase:
  - `authenticate_xnat()` in `R/authenticate.R`
  - `list_experiments()` in `R/list_experiments.R`
  - `download_all_subjects()` in `R/download_files.R`

**Variables:**
- Snake_case for local variables: `project_id`, `base_url`, `ssl_verify`, `dest_dir`
- Descriptive naming: `auth_header`, `response`, `content_json`, `experiments_df`
- Data frames suffixed with `_df`: `projects_df`, `subjects_df`, `experiments_df`
- URLs and paths use full names: `base_url`, `projects_url`, `dest_file`, `dest_dir`
- Examples from `R/download_files.R`:
  - `project_id_enc`, `subject_id_enc` (for URL-encoded parameters)
  - `dest_file`, `dest_dir` (for file paths)
  - `response`, `content_json`, `content_parsed` (for API responses)

**Package-level variables:**
- All caps for environment/package state: `xnatR_env` (defined in `R/environment.R`)
- Stores: `auth_header`, `base_url`, `ssl_verify` as private cached credentials

## Code Style

**Formatting:**
- Indentation: 2 spaces (standard R convention)
- Line length: Generally under 100 characters
- Function definitions use named parameters with default values
- Example from `R/authenticate.R`:
  ```r
  authenticate_xnat <- function(base_url = NULL, username = NULL, password = NULL, token = NULL, ssl_verify = TRUE) {
  ```

**Linting:**
- RoxygenNote: 7.3.1 in DESCRIPTION file
- No explicit linter configuration file detected
- Code follows standard R style conventions

**Spacing and Braces:**
- Space before `{` in conditional blocks: `if (is.null(base_url)) {`
- Space before colons in function parameters: `format = "zip"`
- Pipe operations use base R with `|` operator (not dplyr pipe)
- Example from `R/search_projects.R`: Uses `dplyr::filter()` with pipe-like operations

## Import Organization

**Package Dependencies:**
- Declared in NAMESPACE file with `@import` and `@importFrom` roxygen directives
- Imports list in DESCRIPTION:
  - `httr` - HTTP requests and authentication
  - `jsonlite` - JSON parsing
  - `dplyr` - Data filtering
  - `ggplot2` - Visualization
  - `shiny`, `shinydashboard` - Web interface
  - `plotly` - Interactive plots
  - `base64enc` - Authentication encoding

**Explicit Import Pattern:**
- Functions called with fully qualified namespace: `httr::GET()`, `httr::config()`, `jsonlite::fromJSON()`
- Example from `R/list_projects.R`:
  ```r
  response <- httr::GET(
    url = projects_url,
    httr::add_headers(Authorization = xnatR_env$auth_header),
    ssl_config,
    httr::accept("application/json")
  )
  ```

**Utilities Used:**
- `utils::URLencode()` for encoding parameters in URLs (seen in `R/download_files.R`)
- `base64enc::base64encode()` for authentication header encoding

## Error Handling

**Patterns:**
- `stop()` with `call. = FALSE` for user-friendly errors (hides call stack)
- Example from `R/list_projects.R`:
  ```r
  if (httr::status_code(response) != 200) {
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Failed to retrieve projects.", call. = FALSE)
  }
  ```

**Validation Pattern:**
- Check parameters at function entry with `missing()` and `is.character()` tests
- Example from `R/download_files.R`:
  ```r
  if (missing(project_id) || !is.character(project_id)) {
    stop("Please provide a valid 'project_id' as a character string.", call. = FALSE)
  }
  ```

**Authentication Check:**
- All functions verify `xnatR_env$auth_header` and `xnatR_env$base_url` exist before API calls
- Example from `R/list_subjects.R`:
  ```r
  if (is.null(xnatR_env$auth_header) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Please run `authenticate_xnat()` first.", call. = FALSE)
  }
  ```

**HTTP Status Handling:**
- Check for status code 200 explicitly: `if (httr::status_code(response) != 200)`
- Print response content before stopping for debugging
- Use `warning()` for non-critical failures: `warning("Failed to download files. Status code: ")`

**Try-Catch Pattern:**
- Used in `R/authenticate.R`:
  ```r
  creds <- try(load_credentials(), silent = TRUE)
  if (inherits(creds, "try-error")) {
    stop("Failed to load credentials from config...")
  }
  ```

## Logging

**Framework:** Base R `message()` and `print()` functions

**Patterns:**
- `message()` for informational output: `message("Authentication successful.")`
- `warning()` for non-fatal issues: `warning("Failed to download file. Status code: ")`
- `print()` for error details from API responses
- Example from `R/authenticate.R`:
  ```r
  message("Authentication successful.")
  message("Created destination directory: ", dest_dir)
  warning("Failed to download file. Status code: ", httr::status_code(response))
  ```

**No structured logging:** Uses direct console output, no logging library (log4r, futile.logger, etc.)

## Comments

**When to Comment:**
- File headers identify the file purpose: `# File: R/authenticate.R`
- Comments describe business logic steps before major code blocks
- Inline comments explain non-obvious API endpoint construction
- Examples from `R/download_files.R`:
  ```r
  # Construct the API endpoint
  # URL encode parameters
  # Specific resource download
  # All resources download
  ```

**Roxygen Documentation (`#'`):**
- Every exported function has roxygen2 documentation
- Includes `@param`, `@return`, `@export` tags
- Includes `@examples` section with `\dontrun{` wrapper (requires authentication)
- Example from `R/list_projects.R`:
  ```r
  #' List XNAT Projects
  #'
  #' Retrieves a list of projects from the authenticated XNAT server.
  #'
  #' @return A data frame containing project details (e.g., ID, Name, Description).
  #' @examples
  #' \dontrun{
  #'   authenticate_xnat()
  #'   projects <- list_projects()
  #' }
  #' @export
  ```

## Function Design

**Size:** Most functions 40-80 lines (file size: 56-269 lines per file)
- Larger files contain multiple related functions (e.g., `R/download_files.R` has 3 functions)
- Simple retrieval functions: 50-75 lines
- Complex download functions: 100-140 lines

**Parameters:**
- Named parameters with defaults where applicable
- Example from `R/download_files.R`:
  ```r
  download_files <- function(
    project_id,
    subject_id,
    experiment_id,
    scan_id = "ALL",
    resource = NULL,
    format = "zip",
    dest_dir = getwd()
  )
  ```

**Return Values:**
- Data frames for list operations: `return(projects_df)`
- Invisibly returns response or TRUE for write operations: `invisible(response)`, `invisible(TRUE)`
- NULL for operations that produce side effects
- Example from `R/authenticate.R`:
  ```r
  invisible(TRUE)  # Successful completion, suppress output
  ```

**Function Organization:**
- Input validation first
- Authentication check next
- URL construction/API setup
- HTTP request
- Response parsing
- Return formatted data

## Module Design

**Exports:**
- All exported functions listed in NAMESPACE (generated by roxygen2)
- Exported: `authenticate_xnat`, `download_all_subjects`, `download_files`, `download_subject`, `download_xnat_file`, `download_xnat_files`, `initialize_config`, `list_experiments`, `list_projects`, `list_scans`, `list_subjects`, `load_credentials`, `search_projects`
- Internal: `xnatR_env` (package environment for credential caching)

**Internal Functions:**
- None currently defined as internal (no `@keywords internal` tag)
- All functions are exported for public API

**Barrel Files:**
- Not used; each file contains one primary function with potential helper functions

**Functional Grouping:**
- `authenticate.R` - Authentication and file download
- `config.R` - Configuration and credential management
- `list_*.R` - Data retrieval functions (projects, subjects, experiments, scans)
- `download_*.R` - File download operations
- `search_*.R` - Search utilities
- `environment.R` - Package-level state management

---

*Convention analysis: 2026-01-26*

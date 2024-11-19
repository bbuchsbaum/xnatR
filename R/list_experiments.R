# File: R/list_experiments.R

#' List Experiments for a Subject
#'
#' Retrieves a list of experiments for a specific subject within a project on the authenticated XNAT server.
#'
#' @param project_id The ID of the project.
#' @param subject_id The ID of the subject.
#'
#' @return A data frame containing experiment details (e.g., ID, Label).
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   experiments <- list_experiments(project_id = "TEST", subject_id = "SUBJECT_ID")
#'   print(experiments)
#' }
#' @export
list_experiments <- function(project_id, subject_id) {
  # Validate inputs
  if (missing(project_id) || !is.character(project_id)) {
    stop("Please provide a valid 'project_id' as a character string.", call. = FALSE)
  }
  if (missing(subject_id) || !is.character(subject_id)) {
    stop("Please provide a valid 'subject_id' as a character string.", call. = FALSE)
  }

  # Check authentication
  if (is.null(xnatR_env$auth_header) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Please run 'authenticate_xnat()' first.", call. = FALSE)
  }

  # Construct the API endpoint
  experiments_url <- sprintf(
    "%s/data/projects/%s/subjects/%s/experiments",
    xnatR_env$base_url,
    utils::URLencode(project_id, reserved = TRUE),
    utils::URLencode(subject_id, reserved = TRUE)
  )

  # Set SSL verification
  ssl_config <- if (xnatR_env$ssl_verify) {
    httr::config()
  } else {
    httr::config(ssl_verifypeer = FALSE)
  }

  # Perform the GET request
  response <- httr::GET(
    url = experiments_url,
    httr::add_headers(Authorization = xnatR_env$auth_header),
    ssl_config,
    httr::accept("application/json")
  )

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Failed to retrieve experiments.", call. = FALSE)
  }

  # Parse the response
  content_json <- httr::content(response, as = "text", encoding = "UTF-8")
  content_parsed <- jsonlite::fromJSON(content_json, flatten = TRUE)

  # Extract experiment details
  if (!is.null(content_parsed$ResultSet$Result)) {
    experiments_df <- as.data.frame(content_parsed$ResultSet$Result)
    return(experiments_df)
  } else {
    stop("Unexpected response format when retrieving experiments.", call. = FALSE)
  }
}

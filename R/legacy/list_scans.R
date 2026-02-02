# File: R/list_scans.R

#' List XNAT Scans
#'
#' Retrieves a list of scans from a specific experiment, subject, and project on the authenticated XNAT server.
#'
#' @param project_id The ID of the project (e.g., "TEST").
#' @param subject_id The ID of the subject (e.g., "1").
#' @param experiment_id The ID of the experiment (e.g., "MR1").
#'
#' @return A data frame containing scan details (e.g., ID, Label, Type).
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   scans <- list_scans(project_id = "TEST", subject_id = "1", experiment_id = "MR1")
#'   print(scans)
#' }
#' @export
list_scans <- function(project_id, subject_id, experiment_id) {
  # Validate inputs
  if (missing(project_id) || !is.character(project_id)) {
    stop("Please provide a valid 'project_id' as a character string.", call. = FALSE)
  }
  if (missing(subject_id) || !is.character(subject_id)) {
    stop("Please provide a valid 'subject_id' as a character string.", call. = FALSE)
  }
  if (missing(experiment_id) || !is.character(experiment_id)) {
    stop("Please provide a valid 'experiment_id' as a character string.", call. = FALSE)
  }

  # Check authentication
  if (is.null(xnatR_env$auth_header) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Please run 'authenticate_xnat()' first.", call. = FALSE)
  }

  # Construct the API endpoint
  scans_url <- sprintf(
    "%s/data/projects/%s/subjects/%s/experiments/%s/scans",
    xnatR_env$base_url,
    utils::URLencode(project_id, reserved = TRUE),
    utils::URLencode(subject_id, reserved = TRUE),
    utils::URLencode(experiment_id, reserved = TRUE)
  )

  # Set SSL verification
  ssl_config <- if (xnatR_env$ssl_verify) {
    httr::config()
  } else {
    httr::config(ssl_verifypeer = FALSE)
  }

  # Perform the GET request
  response <- httr::GET(
    url = scans_url,
    httr::add_headers(Authorization = xnatR_env$auth_header),
    ssl_config,
    httr::accept("application/json")
  )

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Failed to retrieve scans.", call. = FALSE)
  }

  # Parse the response
  content_json <- httr::content(response, as = "text", encoding = "UTF-8")
  content_parsed <- jsonlite::fromJSON(content_json, flatten = TRUE)

  # Extract scan details
  if (!is.null(content_parsed$ResultSet$Result)) {
    scans_df <- as.data.frame(content_parsed$ResultSet$Result)
    return(scans_df)
  } else {
    stop("Unexpected response format when retrieving scans.", call. = FALSE)
  }
}

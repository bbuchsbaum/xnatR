
#' List XNAT Subjects
#'
#' Retrieves a list of subjects from a specific project on the authenticated XNAT server.
#'
#' @param project_id The ID of the project (e.g., "TEST").
#'
#' @return A data frame containing subject details (e.g., ID, Label, Gender, Age).
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   subjects <- list_subjects(project_id = "TEST")
#'   print(subjects)
#' }
#' @export
#' @import httr
#' @import jsonlite
list_subjects <- function(project_id) {
  # Validate input
  if (missing(project_id) || !is.character(project_id)) {
    stop("Please provide a valid `project_id` as a character string.", call. = FALSE)
  }

  # Check authentication
  if (is.null(xnatR_env$auth_header) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Please run `authenticate_xnat()` first.", call. = FALSE)
  }

  # Construct the API endpoint
  subjects_url <- sprintf(
    "%s/data/projects/%s/subjects",
    xnatR_env$base_url,
    URLencode(project_id, reserved = TRUE)
  )

  # Perform the GET request
  response <- GET(
    url = subjects_url,
    add_headers(Authorization = xnatR_env$auth_header),
    config(ssl_verifypeer = FALSE),
    accept("application/json"),
    verbose()
  )

  # Check if the request was successful
  if (status_code(response) != 200) {
    print(content(response, as = "text", encoding = "UTF-8"))
    stop("Failed to retrieve subjects.", call. = FALSE)
  }

  # Parse the response
  content_json <- content(response, as = "text", encoding = "UTF-8")
  content_parsed <- fromJSON(content_json, flatten = TRUE)

  # Extract subject details
  if (!is.null(content_parsed$ResultSet$Result)) {
    subjects_df <- as.data.frame(content_parsed$ResultSet$Result)
    return(subjects_df)
  } else {
    stop("Unexpected response format when retrieving subjects.", call. = FALSE)
  }
}

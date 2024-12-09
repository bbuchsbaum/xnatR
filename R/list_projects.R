# File: R/list_projects.R

#' List XNAT Projects
#'
#' Retrieves a list of projects from the authenticated XNAT server.
#'
#' @return A data frame containing project details (e.g., ID, Name, Description).
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   projects <- list_projects()
#'   print(projects)
#' }
#' @export
list_projects <- function() {
  # Check authentication
  if (is.null(xnatR_env$auth_header) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Please run `authenticate_xnat()` first.", call. = FALSE)
  }

  # Construct the API endpoint
  projects_url <- paste0(xnatR_env$base_url, "/data/projects")

  # Set SSL verification
  ssl_config <- if (xnatR_env$ssl_verify) {
    httr::config()
  } else {
    httr::config(ssl_verifypeer = FALSE)
  }

  # Perform the GET request
  response <- httr::GET(
    url = projects_url,
    httr::add_headers(Authorization = xnatR_env$auth_header),
    ssl_config,
    httr::accept("application/json")
  )

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    print(httr::content(response, as = "text", encoding = "UTF-8"))
    stop("Failed to retrieve projects.", call. = FALSE)
  }

  # Parse the response
  content_json <- httr::content(response, as = "text", encoding = "UTF-8")
  content_parsed <- jsonlite::fromJSON(content_json, flatten = TRUE)

  # Extract project details
  if (!is.null(content_parsed$ResultSet$Result)) {
    projects_df <- as.data.frame(content_parsed$ResultSet$Result)
    return(projects_df)
  } else {
    stop("Unexpected response format when retrieving projects.", call. = FALSE)
  }
}

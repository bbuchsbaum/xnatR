# File: R/search_projects.R

#' Search XNAT Projects by Substring
#'
#' Retrieves projects containing a specific substring in their ID or Name.
#'
#' @param substring A character string to search for within project IDs or Names.
#'
#' @return A data frame of projects that match the substring.
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   results <- search_projects(substring = "Test")
#'   print(results)
#' }
#' @export
search_projects <- function(substring) {
  # Validate inputs
  if (missing(substring) || !is.character(substring)) {
    stop("Please provide a valid 'substring' as a character string.", call. = FALSE)
  }

  # Retrieve all projects
  all_projects <- list_projects()

  # Filter projects containing the substring in ID or Name (case-insensitive)
  filtered_projects <- dplyr::filter(
    all_projects,
    grepl(substring, ID, ignore.case = TRUE) | grepl(substring, name, ignore.case = TRUE)
  )

  # Check if any projects match the criteria
  if (nrow(filtered_projects) == 0) {
    warning(paste("No projects found containing the substring '", substring, "'.", sep = ""))
    return(filtered_projects)  # Returns an empty data frame
  }

  return(filtered_projects)
}

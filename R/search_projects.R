

#' Search and Order XNAT Projects
#'
#' Retrieves projects containing a specific substring in their ID or Label and orders them by date.
#'
#' @param substring A character string to search for within project IDs or Labels.
#' @param order_by_date Logical. If `TRUE`, orders the results by the most recent date first. Defaults to `TRUE`.
#' @param date_field The date field to use for ordering (e.g., "creationDate", "lastUpdate"). Defaults to "creationDate".
#'
#' @return A data frame of projects that match the substring, ordered by the specified date field.
#' @examples
#' \dontrun{
#'   authenticate_xnat()
#'   results <- search_projects(substring = "Test", order_by_date = TRUE)
#'   print(results)
#' }
#' @export
#' @import dplyr
search_projects <- function(substring) {
  # Validate inputs
  if (missing(substring) || !is.character(substring)) {
    stop("Please provide a valid `substring` as a character string.", call. = FALSE)
  }

  # Retrieve all projects
  all_projects <- list_projects()


  # Filter projects containing the substring in ID or Label (case-insensitive)
  filtered_projects <- all_projects %>%
    dplyr::filter(
      grepl(substring, ID, ignore.case = TRUE)
    ) %>% arrange(desc(ID))

  # Check if any projects match the criteria
  if (nrow(filtered_projects) == 0) {
    warning(paste("No projects found containing the substring '", substring, "'.", sep = ""))
    return(filtered_projects)  # Returns an empty data frame
  }


  return(filtered_projects)
}

#' Search XNAT Projects
#'
#' Searches for projects containing a substring in their ID or name.
#'
#' @param pattern A character string to search for (case-insensitive).
#' @param columns Character vector of column names to include.
#'
#' @return A tibble of class `xnat_projects` containing matching projects.
#'
#' @examples
#' \dontrun{
#' authenticate_xnat(base_url = "https://central.xnat.org",
#'                   username = "guest", password = "guest")
#'
#' # Search for projects containing "test"
#' results <- search_projects("test")
#'
#' # Search for projects with "MRI" in name
#' mri_projects <- search_projects("MRI")
#' }
#'
#' @export
search_projects <- function(pattern, columns = NULL) {
  check_string(pattern, "pattern")

  # Get all projects
  projects <- list_projects(columns = columns)

  if (nrow(projects) == 0) {
    cli::cli_alert_warning("No projects found.")
    return(projects)
  }

  # Search in ID and name columns (case-insensitive)
  id_match <- if ("ID" %in% names(projects)) {
    grepl(pattern, projects$ID, ignore.case = TRUE)
  } else {
    rep(FALSE, nrow(projects))
  }

  name_match <- if ("name" %in% names(projects)) {
    grepl(pattern, projects$name, ignore.case = TRUE)
  } else {
    rep(FALSE, nrow(projects))
  }

  matches <- id_match | name_match
  result <- projects[matches, , drop = FALSE]

  if (nrow(result) == 0) {
    cli::cli_alert_warning("No projects found matching {.val {pattern}}.")
  }

  class(result) <- class(projects)
  result
}

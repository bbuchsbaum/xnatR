#' List XNAT Projects
#'
#' Retrieves a list of projects from the authenticated XNAT server.
#'
#' @param columns Character vector of column names to include in the result.
#'   Use NULL (default) to return all available columns.
#' @param limit Maximum number of results to return. NULL for no limit.
#' @param offset Number of results to skip. Used for pagination.
#'
#' @return A tibble of class `xnat_projects` containing project details.
#'   Common columns include:
#'   - `ID`: Project identifier
#'   - `name`: Project display name
#'   - `secondary_ID`: Secondary identifier
#'   - `description`: Project description
#'   - `pi_firstname`, `pi_lastname`: Principal investigator
#'
#' @examples
#' \dontrun{
#' authenticate_xnat(base_url = "https://central.xnat.org",
#'                   username = "guest", password = "guest")
#'
#' # List all projects
#' projects <- list_projects()
#'
#' # Get specific columns only
#' projects <- list_projects(columns = c("ID", "name", "description"))
#'
#' # Pagination
#' first_10 <- list_projects(limit = 10)
#' next_10 <- list_projects(limit = 10, offset = 10)
#' }
#'
#' @export
list_projects <- function(columns = NULL, limit = NULL, offset = NULL) {
  query <- list(
    columns = if (!is.null(columns)) paste(columns, collapse = ",") else NULL,
    limit = limit,
    offset = offset
  )

  xnat_get_tibble("data/projects", query = query, class_name = "xnat_projects")
}

#' @export
print.xnat_projects <- function(x, ...) {
  n <- nrow(x)
  cli::cli_h1("XNAT Projects")
  cli::cli_text("{n} project{?s}")

  if (n > 0) {
    # Show key columns if available
    display_cols <- intersect(c("ID", "name", "description"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

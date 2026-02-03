#' List Subjects in a Project
#'
#' Retrieves a list of subjects from a specific project on the XNAT server.
#'
#' @param project_id The project identifier (required).
#' @param columns Character vector of column names to include.
#'   Use NULL (default) to return all available columns.
#' @param limit Maximum number of results to return.
#' @param offset Number of results to skip for pagination.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_subjects` containing subject details.
#'   Common columns include:
#'   - `ID`: Subject identifier
#'   - `label`: Subject label
#'   - `project`: Project ID
#'   - `gender`: Subject gender
#'   - `handedness`: Subject handedness
#'   - `age`: Subject age
#'
#' @examples
#' \dontrun{
#' authenticate_xnat(base_url = "https://central.xnat.org",
#'                   username = "guest", password = "guest")
#'
#' subjects <- list_subjects(project_id = "MyProject")
#'
#' # With pagination
#' subjects <- list_subjects(project_id = "MyProject", limit = 100)
#' }
#'
#' @export
list_subjects <- function(project_id, columns = NULL, limit = NULL, offset = NULL, client = NULL) {
  check_string(project_id, "project_id")

  path <- xnat_path("data/projects", url_encode(project_id), "subjects")

  query <- list(
    columns = if (!is.null(columns)) paste(columns, collapse = ",") else NULL,
    limit = limit,
    offset = offset
  )

  result <- xnat_get_tibble(path, query = query, class_name = "xnat_subjects", client = client)
  attr(result, "project_id") <- project_id
  result
}

#' @export
print.xnat_subjects <- function(x, ...) {
  n <- nrow(x)
  project <- attr(x, "project_id")
  cli::cli_h1("XNAT Subjects")
  if (!is.null(project)) {
    cli::cli_text("Project: {.val {project}}")
  }
  cli::cli_text("{n} subject{?s}")

  if (n > 0) {
    display_cols <- intersect(c("ID", "label", "gender", "age"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

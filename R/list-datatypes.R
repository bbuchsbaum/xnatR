#' List Available Data Types
#'
#' Retrieves all data types (XSI types) available on the XNAT server.
#' These are the schema elements that define different types of data
#' (e.g., xnat:mrSessionData, xnat:subjectData).
#'
#' @return A tibble of class `xnat_datatypes` containing data type information.
#'   Columns include:
#'   - `ELEMENT_NAME`: The XSI type name (e.g., "xnat:mrSessionData")
#'   - Other schema-related columns
#'
#' @examples
#' \dontrun{
#' datatypes <- list_data_types()
#' print(datatypes)
#'
#' # Find all MR-related types
#' mr_types <- datatypes[grep("mr", datatypes$ELEMENT_NAME, ignore.case = TRUE), ]
#' }
#'
#' @export
list_data_types <- function() {
  result <- xnat_get_tibble("data/search/elements", class_name = "xnat_datatypes")
  result
}

#' List Queryable Fields for a Data Type
#'
#' Retrieves the fields that can be queried/searched for a specific XSI type.
#' Useful for building search queries.
#'
#' @param xsi_type The XSI type to query (e.g., "xnat:mrSessionData", "xnat:subjectData").
#'
#' @return A tibble of class `xnat_fields` containing field information.
#'   Common columns include:
#'   - `FIELD_ID`: Field identifier for queries
#'   - `TYPE`: Data type of the field
#'   - `HEADER`: Display name
#'   - `DESC`: Field description
#'
#' @examples
#' \dontrun{
#' # Get queryable fields for MR sessions
#' fields <- list_queryable_fields("xnat:mrSessionData")
#'
#' # Get fields for subjects
#' subject_fields <- list_queryable_fields("xnat:subjectData")
#' }
#'
#' @export
list_queryable_fields <- function(xsi_type) {
  check_string(xsi_type, "xsi_type")

  path <- xnat_path("data/search/elements", url_encode(xsi_type))
  result <- xnat_get_tibble(path, class_name = "xnat_fields")
  attr(result, "xsi_type") <- xsi_type
  result
}

#' @export
print.xnat_datatypes <- function(x, ...) {
  n <- nrow(x)

  cli::cli_h1("XNAT Data Types")
  cli::cli_text("{n} data type{?s}")

  if (n > 0) {
    display_cols <- intersect(c("ELEMENT_NAME"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

#' @export
print.xnat_fields <- function(x, ...) {
  n <- nrow(x)
  xsi_type <- attr(x, "xsi_type")

  cli::cli_h1("XNAT Queryable Fields")
  if (!is.null(xsi_type)) {
    cli::cli_text("Data type: {.val {xsi_type}}")
  }
  cli::cli_text("{n} field{?s}")

  if (n > 0) {
    display_cols <- intersect(c("FIELD_ID", "TYPE", "HEADER", "DESC"), names(x))
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

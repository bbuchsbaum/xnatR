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

# -----------------------------------------------------------------------------
# Advanced Search API
# -----------------------------------------------------------------------------

#' Search XNAT Data
#'
#' Performs an advanced search across XNAT data using the XML search API.
#' This function builds the XML query for you based on simple R syntax.
#'
#' @param root_type The XSI type to search (e.g., "xnat:mrSessionData",
#'   "xnat:subjectData"). Use `list_data_types()` to see available types.
#' @param fields Character vector of field IDs to return. Use
#'   `list_queryable_fields(root_type)` to see available fields.
#' @param criteria A list of search criteria. Each criterion is a list with:
#'   - `field`: Field ID to search
#'   - `comparison`: One of "EQUALS", "LIKE", "GREATER_THAN", "LESS_THAN"
#'   - `value`: Value to compare
#' @param format Output format: "json" (default) or "csv".
#'
#' @return A tibble containing the search results.
#'
#' @examples
#' \dontrun{
#' # Search for all MR sessions in a project
#' results <- xnat_search(
#'   root_type = "xnat:mrSessionData",
#'   fields = c("xnat:mrSessionData/ID", "xnat:mrSessionData/label",
#'              "xnat:mrSessionData/date"),
#'   criteria = list(
#'     list(field = "xnat:mrSessionData/project",
#'          comparison = "EQUALS",
#'          value = "MyProject")
#'   )
#' )
#'
#' # Search for subjects with specific gender
#' subjects <- xnat_search(
#'   root_type = "xnat:subjectData",
#'   fields = c("xnat:subjectData/ID", "xnat:subjectData/label"),
#'   criteria = list(
#'     list(field = "xnat:subjectData/gender", comparison = "EQUALS", value = "M")
#'   )
#' )
#' }
#'
#' @export
xnat_search <- function(root_type, fields, criteria = NULL, format = "json") {
  check_string(root_type, "root_type")
  check_character(fields, "fields")

  # Build XML search document
  xml <- build_search_xml(root_type, fields, criteria)

  # Execute search
  req <- xnat_request("data/search") |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "text/xml") |>
    httr2::req_url_query(format = format) |>
    httr2::req_body_raw(xml, type = "text/xml")

  resp <- httr2::req_perform(req)

  if (format == "json") {
    json <- httr2::resp_body_json(resp)
    parse_xnat_result(json)
  } else {
    httr2::resp_body_string(resp)
  }
}

#' Build XML Search Document
#'
#' Constructs the XML document required by XNAT's search API.
#'
#' @param root_type XSI type
#' @param fields Fields to return
#' @param criteria Search criteria
#' @return XML string
#' @noRd
build_search_xml <- function(root_type, fields, criteria = NULL) {
  # Build search fields
  fields_xml <- vapply(fields, function(f) {
    sprintf('    <xdat:search_field>
      <xdat:element_name>%s</xdat:element_name>
      <xdat:field_ID>%s</xdat:field_ID>
      <xdat:sequence>0</xdat:sequence>
    </xdat:search_field>', root_type, f)
  }, character(1))

  # Build criteria
  criteria_xml <- ""
  if (!is.null(criteria) && length(criteria) > 0) {
    criteria_items <- vapply(criteria, function(c) {
      sprintf('      <xdat:criteria>
        <xdat:schema_field>%s</xdat:schema_field>
        <xdat:comparison_type>%s</xdat:comparison_type>
        <xdat:value>%s</xdat:value>
      </xdat:criteria>', c$field, c$comparison, c$value)
    }, character(1))

    criteria_xml <- sprintf('  <xdat:search_where method="AND">
%s
  </xdat:search_where>', paste(criteria_items, collapse = "\n"))
  }

  # Assemble full XML
  xml <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<xdat:search ID="" allow-diff-columns="0"
  xmlns:xdat="http://nrg.wustl.edu/security"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <xdat:root_element_name>%s</xdat:root_element_name>
  <xdat:search_field>
%s
  </xdat:search_field>
%s
</xdat:search>', root_type, paste(fields_xml, collapse = "\n"), criteria_xml)

  xml
}

#' Create a Search Criterion
#'
#' Helper function to create a search criterion for use with `xnat_search()`.
#'
#' @param field The field ID to search.
#' @param comparison Comparison type: "EQUALS", "LIKE", "GREATER_THAN", "LESS_THAN",
#'   "GREATER_THAN_EQUAL", "LESS_THAN_EQUAL".
#' @param value The value to compare against.
#'
#' @return A list representing the criterion.
#'
#' @examples
#' \dontrun{
#' # Create criteria for a search
#' project_criterion <- search_criterion(
#'   field = "xnat:mrSessionData/project",
#'   comparison = "EQUALS",
#'   value = "MyProject"
#' )
#'
#' date_criterion <- search_criterion(
#'   field = "xnat:mrSessionData/date",
#'   comparison = "GREATER_THAN",
#'   value = "2023-01-01"
#' )
#'
#' # Use in search
#' results <- xnat_search(
#'   root_type = "xnat:mrSessionData",
#'   fields = c("xnat:mrSessionData/ID"),
#'   criteria = list(project_criterion, date_criterion)
#' )
#' }
#'
#' @export
search_criterion <- function(field, comparison, value) {
  check_string(field, "field")
  check_string(comparison, "comparison")

  valid_comparisons <- c("EQUALS", "LIKE", "GREATER_THAN", "LESS_THAN",
                         "GREATER_THAN_EQUAL", "LESS_THAN_EQUAL")
  comparison <- toupper(comparison)

  if (!comparison %in% valid_comparisons) {
    cli::cli_abort(c(
      "Invalid comparison type: {.val {comparison}}",
      "i" = "Valid types: {.val {valid_comparisons}}"
    ))
  }

  list(
    field = field,
    comparison = comparison,
    value = as.character(value)
  )
}

#' Search Builder - Fluent Interface
#'
#' Creates a search builder object for constructing complex searches
#' with a fluent API.
#'
#' @param root_type The XSI type to search.
#'
#' @return A search builder object.
#'
#' @examples
#' \dontrun{
#' # Build and execute a search using fluent API
#' results <- xnat_search_builder("xnat:mrSessionData") |>
#'   search_select("xnat:mrSessionData/ID", "xnat:mrSessionData/label") |>
#'   search_where("xnat:mrSessionData/project", "EQUALS", "MyProject") |>
#'   search_where("xnat:mrSessionData/date", "GREATER_THAN", "2023-01-01") |>
#'   search_execute()
#' }
#'
#' @export
xnat_search_builder <- function(root_type) {
  check_string(root_type, "root_type")

  structure(
    list(
      root_type = root_type,
      fields = character(),
      criteria = list()
    ),
    class = "xnat_search_builder"
  )
}

#' Add Fields to Search Builder
#'
#' @param builder A search builder object.
#' @param ... Field IDs to include in the results.
#'
#' @return The modified search builder (for chaining).
#'
#' @export
search_select <- function(builder, ...) {
  if (!inherits(builder, "xnat_search_builder")) {
    cli::cli_abort("{.arg builder} must be an xnat_search_builder object.")
  }

  new_fields <- c(...)
  builder$fields <- c(builder$fields, new_fields)
  builder
}

#' Add Criterion to Search Builder
#'
#' @param builder A search builder object.
#' @param field Field ID to filter on.
#' @param comparison Comparison type.
#' @param value Value to compare.
#'
#' @return The modified search builder (for chaining).
#'
#' @export
search_where <- function(builder, field, comparison, value) {
  if (!inherits(builder, "xnat_search_builder")) {
    cli::cli_abort("{.arg builder} must be an xnat_search_builder object.")
  }

  criterion <- search_criterion(field, comparison, value)
  builder$criteria <- c(builder$criteria, list(criterion))
  builder
}

#' Execute Search Builder Query
#'
#' @param builder A search builder object.
#' @param format Output format: "json" (default) or "csv".
#'
#' @return A tibble containing the search results.
#'
#' @export
search_execute <- function(builder, format = "json") {
  if (!inherits(builder, "xnat_search_builder")) {
    cli::cli_abort("{.arg builder} must be an xnat_search_builder object.")
  }

  if (length(builder$fields) == 0) {
    cli::cli_abort("No fields selected. Use {.fn search_select} to add fields.")
  }

  xnat_search(
    root_type = builder$root_type,
    fields = builder$fields,
    criteria = if (length(builder$criteria) > 0) builder$criteria else NULL,
    format = format
  )
}

#' @export
print.xnat_search_builder <- function(x, ...) {
  cli::cli_h1("XNAT Search Builder")
  cli::cli_text("Root type: {.val {x$root_type}}")
  cli::cli_text("Fields: {length(x$fields)}")
  cli::cli_text("Criteria: {length(x$criteria)}")

  if (length(x$fields) > 0) {
    cli::cli_h2("Selected Fields")
    cli::cli_ul(x$fields)
  }

  if (length(x$criteria) > 0) {
    cli::cli_h2("Criteria")
    for (c in x$criteria) {
      cli::cli_li("{c$field} {c$comparison} {.val {c$value}}")
    }
  }

  invisible(x)
}

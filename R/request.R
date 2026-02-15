#' Check that authentication credentials are available
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return Invisible TRUE if authenticated
#' @noRd
require_auth <- function(client = NULL) {
  if (!is.null(client)) {
    if (!inherits(client, "xnat_client")) {
      cli::cli_abort("{.arg client} must be an xnat_client.")
    }
    if (is.null(client$base_url) ||
      (is.null(client$jsession) && (is.null(client$username) || is.null(client$password)))) {
      abort_auth_required()
    }
    return(invisible(TRUE))
  }

  if (is.null(xnatR_env$base_url) ||
    (is.null(xnatR_env$jsession) && (is.null(xnatR_env$username) || is.null(xnatR_env$password)))) {
    abort_auth_required()
  }
  invisible(TRUE)
}

#' Build a base XNAT request with authentication
#'
#' Creates an httr2 request with common configuration:
#' - Base URL from package environment
#' - Basic authentication
#' - JSON accept header
#' - SSL verification setting
#' - Retry on transient errors
#'
#' @param path API path (e.g., "data/projects")
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return An httr2 request object
#' @noRd
xnat_request <- function(path, client = NULL) {
  require_auth(client = client)

  base_url <- if (is.null(client)) xnatR_env$base_url else client$base_url
  ssl_verify <- if (is.null(client)) xnatR_env$ssl_verify else client$ssl_verify
  jsession <- if (is.null(client)) xnatR_env$jsession else client$jsession
  username <- if (is.null(client)) xnatR_env$username else client$username
  password <- if (is.null(client)) xnatR_env$password else client$password

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_headers(Accept = "application/json")

  # Prefer session cookie auth when available to avoid sending Basic Auth on
  # every request.
  if (!is.null(jsession) && nzchar(jsession)) {
    req <- httr2::req_headers(req, Cookie = paste0("JSESSIONID=", jsession))
  } else {
    req <- httr2::req_auth_basic(req, username, password)
  }

  req <- req |>
    httr2::req_retry(
      max_tries = 3,
      backoff = ~2,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    httr2::req_error(body = xnat_error_body)

  # Handle SSL verification
  if (!isTRUE(ssl_verify)) {
    req <- httr2::req_options(req, ssl_verifypeer = FALSE)
  }

  req
}

#' Perform a GET request and parse JSON response
#'
#' @param path API path
#' @param query Optional named list of query parameters
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return Parsed JSON as list
#' @noRd
xnat_get <- function(path, query = NULL, client = NULL) {
  req <- xnat_request(path, client = client)

  query <- compact(query %||% list())

  # Ensure XNAT returns JSON payloads on servers that do not honor Accept.
  if (is.null(query$format)) {
    query$format <- "json"
  }

  if (length(query) > 0) {
    req <- httr2::req_url_query(req, !!!query)
  }

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Perform a GET request and parse as XNAT ResultSet tibble
#'
#' @param path API path
#' @param query Optional named list of query parameters
#' @param class_name S3 class name to add to result
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return A tibble with S3 class
#' @noRd
xnat_get_tibble <- function(path, query = NULL, class_name = NULL, client = NULL) {
  json <- xnat_get(path, query, client = client)
  result <- parse_xnat_result(json)

  if (!is.null(class_name)) {
    result <- as_xnat_tibble(result, class_name)
  }

  result
}

#' Perform a POST request
#'
#' @param path API path
#' @param body Request body (will be JSON-encoded if a list)
#' @param query Optional query parameters
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return Parsed JSON response or NULL
#' @noRd
xnat_post <- function(path, body = NULL, query = NULL, client = NULL) {
  req <- xnat_request(path, client = client)

  if (!is.null(query)) {
    query <- compact(query)
    if (length(query) > 0) {
      req <- httr2::req_url_query(req, !!!query)
    }
  }

  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }

  req <- httr2::req_method(req, "POST")
  resp <- httr2::req_perform(req)

  # Return parsed JSON if available, otherwise NULL
  if (httr2::resp_has_body(resp)) {
    tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) NULL
    )
  } else {
    NULL
  }
}

#' Perform a DELETE request
#'
#' @param path API path
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return TRUE if successful
#' @noRd
xnat_delete <- function(path, client = NULL) {
  req <- xnat_request(path, client = client) |>
    httr2::req_method("DELETE")

  httr2::req_perform(req)
  invisible(TRUE)
}

#' Download a file from XNAT
#'
#' @param path API path to the file
#' @param dest_file Destination file path
#' @param query Optional query parameters
#' @param progress Show progress bar (default TRUE)
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @return Invisible dest_file path
#' @noRd
xnat_download <- function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
  req <- xnat_request(path, client = client)

  if (!is.null(query)) {
    query <- compact(query)
    if (length(query) > 0) {
      req <- httr2::req_url_query(req, !!!query)
    }
  }

  if (progress) {
    req <- httr2::req_progress(req)
  }

  httr2::req_perform(req, path = dest_file)
  invisible(dest_file)
}

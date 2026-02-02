#' Remove NULL elements from a list
#' @param x A list
#' @return A list with NULL elements removed
#' @noRd
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Build an XNAT API path
#' @param ... Path components to join
#' @return A character string path
#' @noRd
xnat_path <- function(...) {
  parts <- c(...)
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = "/")
}

#' URL encode a string for XNAT paths
#' @param x A character string to encode
#' @return URL-encoded string
#' @noRd
url_encode <- function(x) {
  utils::URLencode(x, reserved = TRUE)
}

#' Validate that a parameter is a non-empty character string
#' @param x The value to check
#' @param name The parameter name for error messages
#' @noRd
check_string <- function(x, name) {
  if (missing(x) || is.null(x) || !is.character(x) || length(x) != 1 || !nzchar(x)) {
    cli::cli_abort("{.arg {name}} must be a non-empty character string.")
  }
  invisible(x)
}

#' Validate that a parameter is a character vector
#' @param x The value to check
#' @param name The parameter name for error messages
#' @noRd
check_character <- function(x, name) {
  if (missing(x) || is.null(x) || !is.character(x) || length(x) == 0) {
    cli::cli_abort("{.arg {name}} must be a character vector.")
  }
  invisible(x)
}

#' Parse XNAT JSON response
#'
#' XNAT typically wraps results in a ResultSet structure.
#' This function extracts the Result array and converts to tibble.
#'
#' @param json Parsed JSON from response
#' @return A tibble
#' @noRd
parse_xnat_result <- function(json) {
  if (!is.null(json$ResultSet$Result)) {
    result <- json$ResultSet$Result
    if (length(result) == 0) {
      return(tibble::tibble())
    }
    tibble::as_tibble(result)
  } else {
    cli::cli_abort("Unexpected response format from XNAT API.")
  }
}

#' Convert a tibble to an S3-classed tibble
#' @param x A tibble
#' @param class_name The S3 class name to add
#' @return A tibble with additional S3 class
#' @noRd
as_xnat_tibble <- function(x, class_name) {
  class(x) <- c(class_name, class(x))
  x
}

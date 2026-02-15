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
    columns <- json$ResultSet$Columns

    if (length(result) == 0) {
      return(tibble::tibble())
    }

    # Common XNAT shape: list of row records (one named list per row). If we
    # pass this directly to as_tibble(), it gets interpreted as columns and
    # appears transposed. Row-bind first.
    is_named_record <- function(x) {
      is.list(x) && !is.data.frame(x) && !is.null(names(x)) && all(nzchar(names(x)))
    }

    if (is.list(result) && !is.data.frame(result) && length(result) > 0 &&
        is_named_record(result[[1]])) {
      is_record_list <- all(vapply(result, is_named_record, logical(1)))

      if (is_record_list) {
        record_names <- lapply(result, names)
        all_names <- unique(unlist(record_names, use.names = FALSE))
        all_names <- all_names[nzchar(all_names)]

        if (length(all_names) > 0) {
          rows <- lapply(result, function(rec) {
            row <- stats::setNames(vector("list", length(all_names)), all_names)
            row[names(rec)] <- rec

            row <- lapply(row, function(value) {
              if (is.null(value)) {
                return(NA)
              }
              if (is.atomic(value) && length(value) <= 1) {
                return(value)
              }
              list(value)
            })

            as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
          })
          result <- do.call(rbind, rows)
          rownames(result) <- NULL
        }
      }
    }

    # Some XNAT instances return empty column names in Result while exposing
    # real names in ResultSet$Columns metadata. Apply those names first.
    if (!is.null(columns)) {
      columns <- as.character(columns)

      if (is.data.frame(result) && length(columns) == ncol(result)) {
        current_names <- names(result)
        if (is.null(current_names) || any(!nzchar(current_names)) ||
            all(grepl("^\\.\\.\\.[0-9]+$", current_names))) {
          names(result) <- columns
        }
      } else if (is.matrix(result) && length(columns) == ncol(result)) {
        colnames(result) <- columns
      } else if (is.list(result) && !is.data.frame(result) && length(columns) == length(result)) {
        current_names <- names(result)
        if (is.null(current_names) || any(!nzchar(current_names))) {
          names(result) <- columns
        }
      }
    }

    # Some XNAT servers return a transposed-like structure where each outer
    # element is a column and each cell is a named scalar/list. Recover names
    # and flatten where possible before tibble conversion.
    if (is.list(result) && !is.data.frame(result) && length(result) > 0) {
      infer_column_name <- function(col) {
        nms <- names(col)
        if (!is.null(nms)) {
          nms <- unique(nms[nzchar(nms)])
          if (length(nms) == 1) {
            return(nms)
          }
        }

        if (is.list(col) && length(col) > 0) {
          inner_names <- unique(unlist(lapply(col, function(cell) {
            nm <- names(cell)
            if (is.null(nm)) character(0) else nm[nzchar(nm)]
          }), use.names = FALSE))
          if (length(inner_names) == 1) {
            return(inner_names)
          }
        }

        ""
      }

      simplify_column <- function(col) {
        if (!is.list(col) || length(col) == 0) {
          return(col)
        }

        cells <- lapply(col, function(cell) {
          if (is.list(cell) && length(cell) == 1 &&
              is.atomic(cell[[1]]) && length(cell[[1]]) <= 1) {
            return(cell[[1]])
          }
          if (is.atomic(cell) && length(cell) <= 1) {
            return(cell)
          }
          cell
        })

        if (all(vapply(cells, function(x) is.atomic(x) && length(x) <= 1, logical(1)))) {
          return(unlist(cells, use.names = FALSE))
        }

        cells
      }

      current_names <- names(result)
      if (is.null(current_names) || any(!nzchar(current_names)) ||
          all(grepl("^\\.\\.\\.[0-9]+$", current_names))) {
        inferred <- vapply(result, infer_column_name, character(1))
        if (any(nzchar(inferred))) {
          missing_idx <- which(!nzchar(inferred))
          if (length(missing_idx) > 0) {
            inferred[missing_idx] <- paste0("col_", missing_idx)
          }
          names(result) <- make.unique(inferred, sep = "_")
        }
      }

      result <- lapply(result, simplify_column)
    }

    # Repair any remaining invalid names to keep parsing robust.
    suppressMessages(tibble::as_tibble(result, .name_repair = "unique"))
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

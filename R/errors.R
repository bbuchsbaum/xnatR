#' Extract error message from XNAT response body
#'
#' XNAT may return error details in JSON or HTML format.
#' This function attempts to extract a useful message.
#'
#' @param resp An httr2 response object
#' @return A character string with the error message
#' @noRd
xnat_error_body <- function(resp) {
  content_type <- httr2::resp_content_type(resp)

  if (grepl("json", content_type, ignore.case = TRUE)) {
    tryCatch({
      body <- httr2::resp_body_json(resp)
      if (!is.null(body$message)) {
        return(body$message)
      }
      if (!is.null(body$error)) {
        return(body$error)
      }
      # Some XNAT errors are in ResultSet format
      if (!is.null(body$ResultSet$Message)) {
        return(body$ResultSet$Message)
      }
      return(NULL)
    }, error = function(e) NULL)
  }

  if (grepl("html", content_type, ignore.case = TRUE)) {
    tryCatch({
      body <- httr2::resp_body_string(resp)
      # Try to extract title or first meaningful text
      title_match <- regmatches(body, regexpr("<title>([^<]+)</title>", body, ignore.case = TRUE))
      if (length(title_match) > 0) {
        return(gsub("</?title>", "", title_match, ignore.case = TRUE))
      }
      return(NULL)
    }, error = function(e) NULL)
  }

  NULL
}

#' Check if response indicates authentication failure
#' @param resp An httr2 response object
#' @return Logical
#' @noRd
is_auth_error <- function(resp) {
  httr2::resp_status(resp) %in% c(401, 403)
}

#' Abort with authentication error
#' @noRd
abort_auth_required <- function() {
  cli::cli_abort(
    c("Not authenticated.",
      "i" = "Run {.code authenticate_xnat()} first."),
    class = "xnatR_auth_error"
  )
}

#' Abort with connection error
#' @param url The URL that failed
#' @param error The original error
#' @noRd
abort_connection <- function(url, error = NULL) {
  msg <- c("Failed to connect to XNAT server.",
           "x" = "URL: {url}")
  if (!is.null(error)) {
    msg <- c(msg, "i" = conditionMessage(error))
  }
  cli::cli_abort(msg, class = "xnatR_connection_error")
}

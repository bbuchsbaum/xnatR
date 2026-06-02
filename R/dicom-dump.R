#' Dump DICOM header fields for a session or scan
#'
#' Reads DICOM header values directly from the files stored in the XNAT archive
#' using the `/data/services/dicomdump` service, **without downloading the image
#' files**. This is useful for inspecting acquisition parameters (repetition
#' time, echo time, sequence name, and so on) or for filtering scans on real
#' header values before deciding what to download.
#'
#' Only DICOM-backed scans are supported. The service samples header values from
#' the archived files, and the exact columns returned depend on your XNAT
#' version.
#'
#' @param experiment_id The experiment (session) identifier (required).
#' @param scan_id Optional scan identifier. When supplied, the dump is limited
#'   to that scan; otherwise the whole session is dumped.
#' @param fields Optional character vector of DICOM tags to return. Tags may be
#'   given as 8-digit hex (`"00100010"`) or in `"(0010,0010)"` / `"0010,0010"`
#'   form; recognised tags are normalised to the 8-digit hex the service
#'   expects. When `NULL`, all available header fields are returned.
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#'
#' @return A tibble of class `xnat_dicom_dump`. Columns follow the XNAT DICOM
#'   dump ResultSet and typically include the DICOM tag, value representation
#'   (`vr`), description, and value.
#'
#' @examples
#' \dontrun{
#' xnat_connect(base_url = "https://central.xnat.org",
#'              username = "guest", password = "guest")
#'
#' # Dump every header field for a session
#' dicom_dump("OAS1_0001_MR1")
#'
#' # A single scan, only Modality (0008,0060) and Repetition Time (0018,0080)
#' dicom_dump("OAS1_0001_MR1", scan_id = "1",
#'            fields = c("00080060", "00180080"))
#' }
#'
#' @export
dicom_dump <- function(experiment_id, scan_id = NULL, fields = NULL, client = NULL) {
  check_string(experiment_id, "experiment_id")

  # `src` is an archive filesystem path (not a REST path); it is passed as a
  # query parameter and URL-encoded by httr2, so it must not be pre-encoded.
  src <- xnat_path("/archive/experiments", experiment_id)
  if (!is.null(scan_id)) {
    check_string(scan_id, "scan_id")
    src <- xnat_path(src, "scans", scan_id)
  }

  if (!is.null(fields)) {
    check_character(fields, "fields")
    fields <- .normalize_dicom_field(fields)
  }

  # `field` may carry several values; xnat_get() splices the query with
  # `.multi = "explode"`, emitting a repeated `field=` parameter for each tag.
  query <- list(src = src, field = fields)

  result <- xnat_get_tibble(
    "data/services/dicomdump",
    query = query,
    class_name = "xnat_dicom_dump",
    client = client
  )
  attr(result, "experiment_id") <- experiment_id
  attr(result, "scan_id") <- scan_id
  result
}

#' Normalise DICOM tags to the 8-digit hex the dump service expects
#'
#' Strips parentheses, commas, and spaces so that `"(0010,0010)"`,
#' `"0010,0010"`, and `"00100010"` all become `"00100010"`. Values that do not
#' look like a tag (e.g. DICOM keywords) are returned unchanged.
#'
#' @param x Character vector of tags or keywords.
#' @return Character vector the same length as `x`.
#' @noRd
.normalize_dicom_field <- function(x) {
  vapply(
    x,
    function(f) {
      stripped <- gsub("[(), ]", "", f)
      if (grepl("^[0-9A-Fa-f]{8}$", stripped)) toupper(stripped) else f
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' @export
print.xnat_dicom_dump <- function(x, ...) {
  n <- nrow(x)
  experiment <- attr(x, "experiment_id")
  scan <- attr(x, "scan_id")

  cli::cli_h1("XNAT DICOM Dump")
  if (!is.null(experiment)) {
    cli::cli_text("Experiment: {.val {experiment}}")
  }
  if (!is.null(scan)) {
    cli::cli_text("Scan: {.val {scan}}")
  }
  cli::cli_text("{n} field{?s}")

  if (n > 0) {
    display_cols <- intersect(
      c("tag1", "tag", "vr", "desc", "description", "value"),
      names(x)
    )
    if (length(display_cols) > 0) {
      print(tibble::as_tibble(x[, display_cols, drop = FALSE]), ...)
    } else {
      print(tibble::as_tibble(x), ...)
    }
  }

  invisible(x)
}

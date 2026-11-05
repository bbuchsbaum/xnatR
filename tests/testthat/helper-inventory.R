# Synthetic two-project server: repeated visits, missing optional subject fields,
# an unknown date and identity, an empty scan, and non-DICOM resources.
inventory_fixture <- function() {
  tables <- list(
    "data/projects" = tibble::tibble(ID = c("P", "Q"), name = c("Study", "Other")),
    "data/projects/P/subjects" = tibble::tibble(ID = c("S1", "S2"), label = c("person", "person")),
    "data/projects/Q/subjects" = tibble::tibble(ID = "S3", label = "other"),
    "data/projects/P/experiments" = tibble::tibble(ID = c("E1", "E2", "E3", "E4"),
      label = c("visit", "visit", "unknown", "old"), date = c("2026-02-26", "2026-03-02", NA, "2025-01-01")),
    "data/projects/Q/experiments" = tibble::tibble(ID = "E5", label = "other", date = "2026-02-27", subject_ID = "S3"),
    "data/projects/P/subjects/S1/experiments" = tibble::tibble(ID = c("E1", "E2")),
    "data/projects/P/subjects/S2/experiments" = tibble::tibble(ID = "E4"),
    "data/projects/Q/subjects/S3/experiments" = tibble::tibble(ID = "E5")
  )
  for (eid in paste0("E", 1:5)) {
    root <- paste0("data/experiments/", eid, "/scans")
    tables[[root]] <- tibble::tibble(ID = c("1", "2"), type = c("audio", "scout"), series_description = c("sentence audio", NA))
    tables[[paste0(root, "/1/resources")]] <- tibble::tibble(xnat_abstractresource_id = c("100", "101"),
      label = c("DICOM", "SNAPSHOTS"), file_count = c("2", "1"), file_size = c("30", "5"))
    tables[[paste0(root, "/2/resources")]] <- tibble::tibble()
    tables[[paste0(root, "/1/resources/100/files")]] <- tibble::tibble(Name = c("same.dcm", "same.dcm"),
      URI = paste0("/data/experiments/", eid, "/scans/1/resources/100/files/", c("a/same.dcm", "b/same.dcm")), Size = c("10", "20"))
  }
  tables
}

fixture_get <- function(tables, legacy = FALSE, cap = Inf, totals = FALSE) {
  force(tables)
  function(path, query = list(), client = NULL) {
    if (is.null(tables[[path]])) stop(paste("Unexpected fixture path:", path))
    all <- tables[[path]]
    rows <- all
    if (!legacy && !identical(query$limit, "*")) {
      from <- (query$offset %||% 0) + 1L
      to <- min(nrow(rows), from + min(query$limit %||% Inf, cap) - 1L)
      rows <- rows[if (from <= to) seq.int(from, to) else integer(), , drop = FALSE]
    }
    result <- list(ResultSet = list(Result = rows))
    if (totals || legacy || identical(query$limit, "*")) result$ResultSet$totalRecords <- as.character(nrow(all))
    result
  }
}

fixture_inventory <- function(...) xnat_inventory(client = xnat_client("https://example.org", "u", "p"), page_size = 2L, ...)
fixture_plan <- function(inventory, ids = c("E1", "E2"), ...) {
  xnat_plan_download(inventory, ids, file.path(tempdir(), "planned-archives"),
    client = xnat_client("https://example.org", "u", "p"), ...)
}

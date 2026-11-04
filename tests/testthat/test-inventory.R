test_that("pagination completes capped pages without totals and validates reported totals", {
  tables <- list(test = tibble::tibble(ID = as.character(1:5)))
  local_mocked_bindings(xnat_get = fixture_get(tables, cap = 1))
  rows <- xnat_collect("test", page_size = 3)
  expect_equal(rows$ID, as.character(1:5))
  expect_equal(attr(rows, "pagination")$requests, 6L)
  local_mocked_bindings(xnat_get = fixture_get(tables, totals = TRUE))
  expect_equal(nrow(xnat_collect("test", page_size = 2)), 5)
})

test_that("ignored offsets use only a checked legacy all-results fallback", {
  tables <- list(test = tibble::tibble(ID = c("1", "2")))
  local_mocked_bindings(xnat_get = fixture_get(tables, legacy = TRUE))
  result <- xnat_collect("test", page_size = 10)
  expect_equal(result$ID, c("1", "2"))
  expect_equal(attr(result, "pagination")$mode, "all")
  expect_error(xnat_collect("test", pagination = "paged"), "Overlapping")
  local_mocked_bindings(xnat_get = function(...) list(ResultSet = list(Result = tables$test)))
  expect_error(xnat_collect("test"), "totalRecords")
})

test_that("incomplete, overlapping, changing and duplicate collections are refused", {
  local_mocked_bindings(xnat_get = function(path, query, ...) {
    list(ResultSet = list(Result = if (query$offset == 0) tibble::tibble(ID = "1") else tibble::tibble(), totalRecords = "2"))
  })
  expect_error(xnat_collect("test"), "before totalRecords")
  local_mocked_bindings(xnat_get = function(path, query, ...) {
    list(ResultSet = list(Result = tibble::tibble(ID = as.character(query$offset + 1:2))))
  })
  expect_error(xnat_collect("test", page_size = 2, max_pages = 2), "max_pages")
  local_mocked_bindings(xnat_get = function(...) list(ResultSet = list(Result = tibble::tibble(ID = c("1", "1")))))
  expect_error(xnat_collect("test"), "duplicate")
  local_mocked_bindings(xnat_get = function(path, query, ...) {
    list(ResultSet = list(Result = tibble::tibble(ID = if (query$offset == 0) c("1", "2") else c("2", "3"))))
  })
  expect_error(xnat_collect("test", page_size = 2), "Overlapping")
})

test_that("date-filtered discovery resolves participants without interpreting labels", {
  local_mocked_bindings(xnat_get = fixture_get(inventory_fixture(), cap = 1))
  inv <- fixture_inventory(date_from = "2026-02-01", date_to = as.Date("2026-03-31"))
  expect_s3_class(inv, "xnat_inventory")
  expect_setequal(inv$sessions$experiment_id, c("E1", "E2", "E3", "E5"))
  expect_equal(inv$sessions$subject_id, c("S1", "S1", NA, "S3"))
  expect_equal(inv$sessions$subject_status, c("resolved", "resolved", "unknown", "reported"))
  expect_equal(inv$sessions$participant_session_count, c(2L, 2L, NA_integer_, 1L))
  expect_equal(inv$sessions$date_status, c("known", "known", "unknown", "known"))
  expect_equal(inv$sessions$scan_count, rep(2L, 4))
  expect_equal(inv$sessions$scan_types, rep("audio; scout", 4))
  expect_equal(nrow(inv$resources), 8)
  expect_true(all(vapply(inv$collections, function(x) x$mode == "paged", logical(1))))
})

test_that("date and series filtering keep all scans of matching sessions", {
  tables <- inventory_fixture()
  tables[["data/experiments/E2/scans"]]$type <- c("T1", "scout")
  tables[["data/experiments/E2/scans"]]$series_description <- NA_character_
  local_mocked_bindings(xnat_get = fixture_get(tables))
  inv <- fixture_inventory(project_ids = "P", date_from = "2026-02-26", date_to = "2026-03-02",
    series_pattern = "AUDIO", include_unknown_dates = FALSE)
  expect_equal(inv$sessions$experiment_id, "E1")
  expect_equal(inv$scans$scan_id, c("1", "2"))
  expect_equal(inv$scans$series_match, c(TRUE, FALSE))
  expect_equal(inv$sessions$participant_session_count, 1L)
  expect_error(fixture_inventory(date_from = "2026-02-30"), "valid ISO")
  expect_error(fixture_inventory(date_from = "2026-04-01", date_to = "2026-03-01"), "exceeds")
  expect_error(fixture_inventory(project_ids = "NotAnID"), "canonical project")
})

test_that("empty discovery remains a typed inventory", {
  local_mocked_bindings(xnat_get = fixture_get(list("data/projects" = tibble::tibble())))
  inv <- fixture_inventory()
  expect_equal(nrow(inv$sessions), 0)
  expect_type(inv$sessions$scan_count, "integer")
  expect_type(inv$scans$series_match, "logical")
  expect_no_error(capture.output(print(inv)))
})

test_that("conflicting membership and permission failures cannot become unknown or complete", {
  tables <- inventory_fixture()
  tables[["data/projects/P/subjects/S2/experiments"]] <- tibble::tibble(ID = "E1")
  local_mocked_bindings(xnat_get = fixture_get(tables))
  expect_error(fixture_inventory(), "Conflicting")
  original <- fixture_get(inventory_fixture())
  local_mocked_bindings(xnat_get = function(path, ...) {
    if (path == "data/projects/P/subjects/S2/experiments") transport_abort("permission", 403)
    original(path, ...)
  })
  expect_error(fixture_inventory(), class = "xnatR_permission_error")
})

test_that("JSON integer IDs and sizes are decoded without double rounding", {
  payload <- '{"ResultSet":{"Result":[{"ID":9007199254740993,"Size":9007199254740995}]}}'
  local_mocked_bindings(xnat_perform = function(...) httr2::response(
    headers = list(`content-type` = "application/json"), body = charToRaw(payload)))
  client <- xnat_client("https://example.org", "u", "p")
  table <- parse_xnat_result(xnat_get("data/projects", client = client))
  expect_identical(table$ID, "9007199254740993")
  expect_identical(table$Size, "9007199254740995")
})

test_that("changing totals and inconsistent legacy fallback cannot pass", {
  local_mocked_bindings(xnat_get = function(path, query, ...) {
    list(ResultSet = list(Result = tibble::tibble(ID = as.character(query$offset + 1)),
      totalRecords = if (query$offset == 0) "2" else "3"))
  })
  expect_error(xnat_collect("test"), "totalRecords changed")
  local_mocked_bindings(xnat_get = function(path, query, ...) {
    list(ResultSet = list(Result = tibble::tibble(ID = "1", label = if (identical(query$limit, "*")) "changed" else "original"), totalRecords = "1"))
  })
  expect_error(xnat_collect("test"), "changed during pagination")
})

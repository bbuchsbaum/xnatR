file_catalog_fixture <- function() {
  list(ResultSet = list(title = "Files", Result = tibble::tibble(
    Name = c("a.dcm", "b.dcm"), Size = c("10", "20"),
    URI = paste0("/data/experiments/E/scans/1/resources/100/files/", c("a.dcm", "b.dcm")))))
}

test_that("scan file catalogs use the all-files contract without a totalRecords field", {
  calls <- 0L
  local_mocked_bindings(xnat_get = function(path, query, client) {
    calls <<- calls + 1L
    expect_identical(query, list(limit = "*"))
    file_catalog_fixture()
  })
  rows <- xnat_collect("data/experiments/E/scans/1/resources/100/files", "URI", page_size = 1L)
  expect_identical(rows$Name, c("a.dcm", "b.dcm"))
  expect_equal(calls, 1L)
  expect_equal(attr(rows, "pagination")$mode, "resource_files")
  expect_equal(attr(rows, "pagination")$rows, 2L)
})

test_that("file catalogs still refuse malformed responses, duplicate URIs and false counts", {
  path <- "data/experiments/E/scans/1/resources/100/files"
  response <- file_catalog_fixture()
  local_mocked_bindings(xnat_get = function(...) response)
  response$ResultSet$totalRecords <- "3"
  expect_error(xnat_collect(path, "URI"), "totalRecords")
  response$ResultSet$totalRecords <- "0"
  expect_error(xnat_collect(path, "URI"), "totalRecords")
  response$ResultSet$totalRecords <- "invalid"
  expect_error(xnat_collect(path, "URI"), "totalRecords")
  response$ResultSet$totalRecords <- "2"
  expect_equal(nrow(xnat_collect(path, "URI")), 2L)
  response$ResultSet$totalRecords <- NULL
  response$ResultSet$Result$URI[2] <- response$ResultSet$Result$URI[1]
  expect_error(xnat_collect(path, "URI"), "duplicate")
  response <- file_catalog_fixture()
  response$ResultSet$title <- "Other"
  expect_error(xnat_collect(path, "URI"), "file catalog")
  response$ResultSet$Result <- NULL
  expect_error(xnat_collect(path, "URI"), "file catalog")
  response <- list()
  expect_error(xnat_collect(path, "URI"), "file catalog")
})

test_that("all-files handling does not weaken paged or unrelated collection requests", {
  local_mocked_bindings(xnat_get = function(...) file_catalog_fixture())
  expect_error(xnat_collect("data/experiments/E/scans/1/resources/100/files", "URI", pagination = "paged"), "Overlapping")
  expect_error(xnat_collect("data/experiments/E/scans/1/resources/100/files", "URI", pagination = "all"), "totalRecords")
  expect_error(xnat_collect("data/experiments/E/scans/1/resources/100/files", "URI", query = list(tag = "subset")), "totalRecords")
  expect_error(xnat_collect("data/experiments/E/resources/100/files", "URI"), "totalRecords")
  expect_error(xnat_collect("data/experiments/E/scans/1/resources/100/files/child", "URI"), "totalRecords")
})

test_that("a declared empty file catalog is distinct from a missing Result", {
  local_mocked_bindings(xnat_get = function(...) list(ResultSet = list(title = "Files", Result = list())))
  rows <- xnat_collect("data/experiments/E/scans/1/resources/100/files", "URI")
  expect_equal(nrow(rows), 0L)
  expect_equal(attr(rows, "pagination")$mode, "resource_files")
})

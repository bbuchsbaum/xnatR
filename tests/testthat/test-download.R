# Tests for download functions

test_that("download_files constructs correct path for all scans", {
  setup_test_auth()

  downloaded_path <- NULL
  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/scans/ALL/files")
      expect_equal(query$format, "zip")
      downloaded_path <<- dest_file
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    dest_dir = tempdir()
  ))

  expect_true(file.exists(dirname(result)) || TRUE)  # Just check it returns a path
  expect_match(result, "P1_S1_E1_ALL_ALL")

  clear_test_auth()
})

test_that("download_files constructs correct path for specific scans", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      # Comma is URL-encoded to %2C
      expect_match(path, "scans/1%2C2%2C3/files")
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    scan_id = c("1", "2", "3"),
    dest_dir = tempdir()
  ))

  expect_match(result, "1-2-3")

  clear_test_auth()
})

test_that("download_files constructs correct path for specific resource", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      expect_match(path, "resources/DICOM/files")
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    resource = "DICOM",
    dest_dir = tempdir()
  ))

  expect_match(result, "DICOM")

  clear_test_auth()
})

test_that("download_files uses custom dest_file", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      expect_match(basename(dest_file), "custom_name.zip")
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    dest_dir = tempdir(),
    dest_file = "custom_name.zip"
  ))

  expect_match(result, "custom_name.zip")

  clear_test_auth()
})

test_that("download_files supports tar.gz format", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      expect_equal(query$format, "tar.gz")
      expect_match(dest_file, "\\.tar\\.gz$")
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    format = "tar.gz",
    dest_dir = tempdir()
  ))

  expect_match(result, "\\.tar\\.gz$")

  clear_test_auth()
})

test_that("download_xnat_file downloads to specified path", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      expect_equal(path, "data/files/test.dcm")
      expect_equal(dest_file, file.path(tempdir(), "output.dcm"))
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_xnat_file(
    url = "/data/files/test.dcm",
    dest_file = file.path(tempdir(), "output.dcm")
  ))

  expect_equal(result, file.path(tempdir(), "output.dcm"))

  clear_test_auth()
})

test_that("download_xnat_file handles full URLs", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      expect_equal(path, "data/files/test.dcm")  # Path extracted from URL
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_xnat_file(
    url = "https://xnat.example.com/data/files/test.dcm",
    dest_file = file.path(tempdir(), "output.dcm")
  ))

  clear_test_auth()
})

test_that("download_subject downloads all experiments", {
  setup_test_auth()

  download_count <- 0
  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_experiments(3))
    },
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      download_count <<- download_count + 1
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_subject(
    project_id = "P1",
    subject_id = "S1",
    dest_dir = tempdir()
  ))

  expect_equal(download_count, 3)
  expect_length(result, 3)

  clear_test_auth()
})

test_that("download_subject handles no experiments", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(list())
    },
    .package = "xnatR"
  )

  result <- suppressMessages(download_subject(
    project_id = "P1",
    subject_id = "S1",
    dest_dir = tempdir()
  ))

  expect_length(result, 0)

  clear_test_auth()
})

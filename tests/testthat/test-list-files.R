# Tests for list_files and list_experiment_files

test_that("list_files returns xnat_files tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/scans/1/resources/DICOM/files")
      mock_resultset(mock_files(5))
    },
    .package = "xnatR"
  )

  result <- list_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    scan_id = "1",
    resource = "DICOM"
  )

  expect_s3_class(result, "xnat_files")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  expect_equal(attr(result, "resource"), "DICOM")
  expect_true("Name" %in% names(result))
  expect_true("Size" %in% names(result))

  clear_test_auth()
})

test_that("list_experiment_files returns xnat_files tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/resources/SNAPSHOTS/files")
      expect_false(grepl("scans", path))
      mock_resultset(mock_files(2))
    },
    .package = "xnatR"
  )

  result <- list_experiment_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    resource = "SNAPSHOTS"
  )

  expect_s3_class(result, "xnat_files")
  expect_equal(nrow(result), 2)
  expect_null(attr(result, "scan_id"))
  expect_equal(attr(result, "resource"), "SNAPSHOTS")

  clear_test_auth()
})

test_that("print.xnat_files shows resource", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_files(10))
    },
    .package = "xnatR"
  )

  result <- list_files(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    scan_id = "1",
    resource = "NIFTI"
  )

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "resource"), "NIFTI")
  expect_equal(nrow(result), 10)

  clear_test_auth()
})

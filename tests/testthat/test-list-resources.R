# Tests for list_resources and list_experiment_resources

test_that("list_resources returns xnat_resources tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/scans/1/resources")
      mock_resultset(mock_resources(2))
    },
    .package = "xnatR"
  )

  result <- list_resources(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    scan_id = "1"
  )

  expect_s3_class(result, "xnat_resources")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(attr(result, "scan_id"), "1")
  expect_true("label" %in% names(result))

  clear_test_auth()
})

test_that("list_experiment_resources returns xnat_resources tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/resources")
      expect_false(grepl("scans", path))
      mock_resultset(mock_resources(1))
    },
    .package = "xnatR"
  )

  result <- list_experiment_resources(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1"
  )

  expect_s3_class(result, "xnat_resources")
  expect_equal(nrow(result), 1)
  expect_null(attr(result, "scan_id"))

  clear_test_auth()
})

test_that("print.xnat_resources shows context", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_resources(3))
    },
    .package = "xnatR"
  )

  result <- list_resources(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    scan_id = "scan_5"
  )

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "scan_id"), "scan_5")
  expect_equal(nrow(result), 3)

  clear_test_auth()
})

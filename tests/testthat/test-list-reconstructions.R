# Tests for list_reconstructions

test_that("list_reconstructions returns xnat_reconstructions tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/reconstructions")
      mock_resultset(mock_reconstructions(2))
    },
    .package = "xnatR"
  )

  result <- list_reconstructions(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1"
  )

  expect_s3_class(result, "xnat_reconstructions")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(attr(result, "project_id"), "P1")
  expect_equal(attr(result, "experiment_id"), "E1")
  expect_true("ID" %in% names(result))

  clear_test_auth()
})

test_that("list_reconstructions passes query parameters", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$columns, "ID,type")
      expect_equal(query$limit, 10)
      expect_equal(query$offset, 5)
      mock_resultset(mock_reconstructions(1))
    },
    .package = "xnatR"
  )

  result <- list_reconstructions(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    columns = c("ID", "type"),
    limit = 10,
    offset = 5
  )
  expect_equal(nrow(result), 1)

  clear_test_auth()
})

test_that("print.xnat_reconstructions shows context", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_reconstructions(1))
    },
    .package = "xnatR"
  )

  result <- list_reconstructions(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "TestExp"
  )

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "experiment_id"), "TestExp")
  expect_equal(nrow(result), 1)

  clear_test_auth()
})

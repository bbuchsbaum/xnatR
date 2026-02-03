# Tests for list_assessors

test_that("list_assessors returns xnat_assessors tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/assessors")
      mock_resultset(mock_assessors(2))
    },
    .package = "xnatR"
  )

  result <- list_assessors(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1"
  )

  expect_s3_class(result, "xnat_assessors")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(attr(result, "project_id"), "P1")
  expect_equal(attr(result, "experiment_id"), "E1")
  expect_true("ID" %in% names(result))
  expect_true("xsiType" %in% names(result))

  clear_test_auth()
})

test_that("list_assessors passes query parameters", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$columns, "ID,label")
      expect_equal(query$limit, 5)
      mock_resultset(mock_assessors(2))
    },
    .package = "xnatR"
  )

  result <- list_assessors(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    columns = c("ID", "label"),
    limit = 5
  )
  expect_equal(nrow(result), 2)

  clear_test_auth()
})

test_that("print.xnat_assessors shows experiment context", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_assessors(3))
    },
    .package = "xnatR"
  )

  result <- list_assessors(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "MyExperiment"
  )

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "experiment_id"), "MyExperiment")
  expect_equal(nrow(result), 3)

  clear_test_auth()
})

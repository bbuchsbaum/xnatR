# Tests for list_scans

test_that("list_scans returns xnat_scans tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments/E1/scans")
      mock_resultset(mock_scans(5))
    },
    .package = "xnatR"
  )

  result <- list_scans(project_id = "P1", subject_id = "S1", experiment_id = "E1")

  expect_s3_class(result, "xnat_scans")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  expect_equal(attr(result, "project_id"), "P1")
  expect_equal(attr(result, "subject_id"), "S1")
  expect_equal(attr(result, "experiment_id"), "E1")
  expect_true("ID" %in% names(result))
  expect_true("type" %in% names(result))

  clear_test_auth()
})

test_that("list_scans passes query parameters", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$columns, "ID,type,quality")
      expect_equal(query$limit, 10)
      expect_equal(query$offset, 0)
      mock_resultset(mock_scans(3))
    },
    .package = "xnatR"
  )

  result <- list_scans(
    project_id = "P1",
    subject_id = "S1",
    experiment_id = "E1",
    columns = c("ID", "type", "quality"),
    limit = 10,
    offset = 0
  )
  expect_equal(nrow(result), 3)

  clear_test_auth()
})

test_that("print.xnat_scans shows full context", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_scans(4))
    },
    .package = "xnatR"
  )

  result <- list_scans(project_id = "Proj", subject_id = "Subj", experiment_id = "Exp")

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "project_id"), "Proj")
  expect_equal(attr(result, "subject_id"), "Subj")
  expect_equal(attr(result, "experiment_id"), "Exp")
  expect_equal(nrow(result), 4)

  clear_test_auth()
})

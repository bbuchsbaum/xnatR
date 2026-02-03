# Tests for list_experiments

test_that("list_experiments returns xnat_experiments tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/P1/subjects/S1/experiments")
      mock_resultset(mock_experiments(2, "P1", "S1"))
    },
    .package = "xnatR"
  )

  result <- list_experiments(project_id = "P1", subject_id = "S1")

  expect_s3_class(result, "xnat_experiments")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(attr(result, "project_id"), "P1")
  expect_equal(attr(result, "subject_id"), "S1")
  expect_true("ID" %in% names(result))
  expect_true("xsiType" %in% names(result))

  clear_test_auth()
})

test_that("list_experiments URL-encodes parameters", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "P%2F1")  # URL-encoded slash
      expect_match(path, "S%2F1")
      mock_resultset(mock_experiments(1))
    },
    .package = "xnatR"
  )

  result <- list_experiments(project_id = "P/1", subject_id = "S/1")
  expect_equal(nrow(result), 1)

  clear_test_auth()
})

test_that("list_experiments passes columns and pagination", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$columns, "ID,label,date")
      expect_equal(query$limit, 20)
      mock_resultset(mock_experiments(5))
    },
    .package = "xnatR"
  )

  result <- list_experiments(
    project_id = "P1",
    subject_id = "S1",
    columns = c("ID", "label", "date"),
    limit = 20
  )
  expect_equal(nrow(result), 5)

  clear_test_auth()
})

test_that("print.xnat_experiments shows context", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_experiments(3))
    },
    .package = "xnatR"
  )

  result <- list_experiments(project_id = "MyProject", subject_id = "MySubject")

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "project_id"), "MyProject")
  expect_equal(attr(result, "subject_id"), "MySubject")
  expect_equal(nrow(result), 3)

  clear_test_auth()
})

# Tests for list_subjects

test_that("list_subjects returns xnat_subjects tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/projects/PROJECT_1/subjects")
      mock_resultset(mock_subjects(3, "PROJECT_1"))
    },
    .package = "xnatR"
  )

  result <- list_subjects(project_id = "PROJECT_1")

  expect_s3_class(result, "xnat_subjects")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(attr(result, "project_id"), "PROJECT_1")
  expect_true("ID" %in% names(result))
  expect_true("label" %in% names(result))

  clear_test_auth()
})

test_that("list_subjects URL-encodes project_id", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "PROJECT%20WITH%20SPACE")
      mock_resultset(mock_subjects(1))
    },
    .package = "xnatR"
  )

  result <- list_subjects(project_id = "PROJECT WITH SPACE")
  expect_equal(nrow(result), 1)

  clear_test_auth()
})

test_that("list_subjects passes pagination parameters", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$limit, 50)
      expect_equal(query$offset, 100)
      mock_resultset(mock_subjects(50))
    },
    .package = "xnatR"
  )

  result <- list_subjects(project_id = "P1", limit = 50, offset = 100)
  expect_equal(nrow(result), 50)

  clear_test_auth()
})

test_that("print.xnat_subjects shows project info", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_subjects(2))
    },
    .package = "xnatR"
  )

  result <- list_subjects(project_id = "TEST_PROJECT")

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "project_id"), "TEST_PROJECT")
  expect_equal(nrow(result), 2)

  clear_test_auth()
})

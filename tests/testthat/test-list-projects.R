# Tests for list_projects

test_that("list_projects returns xnat_projects tibble", {
  setup_test_auth()

  # Mock the HTTP response
  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(path, "data/projects")
      mock_resultset(mock_projects(3))
    },
    .package = "xnatR"
  )

  result <- list_projects()

  expect_s3_class(result, "xnat_projects")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("ID" %in% names(result))
  expect_true("name" %in% names(result))

  clear_test_auth()
})

test_that("list_projects passes columns parameter", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$columns, "ID,name")
      mock_resultset(mock_projects(2))
    },
    .package = "xnatR"
  )

  result <- list_projects(columns = c("ID", "name"))
  expect_equal(nrow(result), 2)

  clear_test_auth()
})

test_that("list_projects passes pagination parameters", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$limit, 10)
      expect_equal(query$offset, 5)
      mock_resultset(mock_projects(10))
    },
    .package = "xnatR"
  )

  result <- list_projects(limit = 10, offset = 5)
  expect_equal(nrow(result), 10)

  clear_test_auth()
})

test_that("list_projects returns empty tibble when no projects", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(list())
    },
    .package = "xnatR"
  )

  result <- list_projects()
  expect_s3_class(result, "xnat_projects")
  expect_equal(nrow(result), 0)

  clear_test_auth()
})

test_that("print.xnat_projects works correctly", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_projects(3))
    },
    .package = "xnatR"
  )

  result <- list_projects()

  # Print should not error and should produce output
  expect_no_error(capture.output(print(result), type = "output"))

  # Check the tibble data is correct
  expect_equal(nrow(result), 3)
  expect_true("ID" %in% names(result))

  clear_test_auth()
})

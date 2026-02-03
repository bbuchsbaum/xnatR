# Tests for list_data_types and list_queryable_fields

test_that("list_data_types returns xnat_datatypes tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(path, "data/search/elements")
      mock_resultset(mock_datatypes())
    },
    .package = "xnatR"
  )

  result <- list_data_types()

  expect_s3_class(result, "xnat_datatypes")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_true("ELEMENT_NAME" %in% names(result))

  clear_test_auth()
})

test_that("list_queryable_fields returns xnat_fields tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      # Colon is URL-encoded as %3A
      expect_match(path, "data/search/elements/xnat%3AmrSessionData")
      mock_resultset(mock_fields())
    },
    .package = "xnatR"
  )

  result <- list_queryable_fields("xnat:mrSessionData")

  expect_s3_class(result, "xnat_fields")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_equal(attr(result, "xsi_type"), "xnat:mrSessionData")
  expect_true("FIELD_ID" %in% names(result))
  expect_true("TYPE" %in% names(result))

  clear_test_auth()
})

test_that("list_queryable_fields URL-encodes xsi_type", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "xnat%3AsubjectData")  # URL-encoded colon
      mock_resultset(mock_fields())
    },
    .package = "xnatR"
  )

  result <- list_queryable_fields("xnat:subjectData")
  expect_s3_class(result, "xnat_fields")

  clear_test_auth()
})

test_that("print.xnat_datatypes works", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_datatypes())
    },
    .package = "xnatR"
  )

  result <- list_data_types()

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify data
  expect_equal(nrow(result), 4)

  clear_test_auth()
})

test_that("print.xnat_fields shows xsi_type", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_fields())
    },
    .package = "xnatR"
  )

  result <- list_queryable_fields("xnat:mrSessionData")

  # Print should not error
  expect_no_error(capture.output(print(result), type = "output"))

  # Verify attributes
  expect_equal(attr(result, "xsi_type"), "xnat:mrSessionData")
  expect_equal(nrow(result), 4)

  clear_test_auth()
})

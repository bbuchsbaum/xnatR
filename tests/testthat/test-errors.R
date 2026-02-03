# Tests for error handling functions

test_that("xnat_error_body extracts message from JSON response", {
  # Create a mock response with JSON body
  mock_resp <- structure(
    list(
      headers = list(`content-type` = "application/json"),
      body = charToRaw('{"message": "Test error message"}')
    ),
    class = "httr2_response"
  )

  local_mocked_bindings(
    resp_content_type = function(resp) "application/json",
    resp_body_json = function(resp) list(message = "Test error message"),
    .package = "httr2"
  )

  result <- xnatR:::xnat_error_body(mock_resp)
  expect_equal(result, "Test error message")
})

test_that("xnat_error_body extracts error field from JSON", {
  mock_resp <- structure(list(), class = "httr2_response")

  local_mocked_bindings(
    resp_content_type = function(resp) "application/json",
    resp_body_json = function(resp) list(error = "Error field content"),
    .package = "httr2"
  )

  result <- xnatR:::xnat_error_body(mock_resp)
  expect_equal(result, "Error field content")
})

test_that("xnat_error_body extracts ResultSet message", {
  mock_resp <- structure(list(), class = "httr2_response")

  local_mocked_bindings(
    resp_content_type = function(resp) "application/json",
    resp_body_json = function(resp) list(ResultSet = list(Message = "ResultSet message")),
    .package = "httr2"
  )

  result <- xnatR:::xnat_error_body(mock_resp)
  expect_equal(result, "ResultSet message")
})

test_that("xnat_error_body returns NULL for empty content type", {
  mock_resp <- structure(list(), class = "httr2_response")

  local_mocked_bindings(
    resp_content_type = function(resp) "",
    .package = "httr2"
  )

  result <- xnatR:::xnat_error_body(mock_resp)
  expect_null(result)
})

test_that("xnat_error_body handles content type errors gracefully", {
  mock_resp <- structure(list(), class = "httr2_response")

  local_mocked_bindings(
    resp_content_type = function(resp) stop("No content type"),
    .package = "httr2"
  )

  result <- xnatR:::xnat_error_body(mock_resp)
  expect_null(result)
})

test_that("abort_auth_required throws xnatR_auth_error", {
  expect_error(
    xnatR:::abort_auth_required(),
    class = "xnatR_auth_error"
  )
  expect_error(
    xnatR:::abort_auth_required(),
    "Not authenticated"
  )
})

test_that("abort_connection includes URL in message", {
  expect_error(
    xnatR:::abort_connection("https://test.xnat.org"),
    class = "xnatR_connection_error"
  )
  expect_error(
    xnatR:::abort_connection("https://test.xnat.org"),
    "test.xnat.org"
  )
})

test_that("abort_connection includes original error message", {
  original_error <- simpleError("Connection refused")
  expect_error(
    xnatR:::abort_connection("https://test.xnat.org", original_error),
    "Connection refused"
  )
})

test_that("is_auth_error identifies 401 status", {
  mock_resp <- structure(list(), class = "httr2_response")

  local_mocked_bindings(
    resp_status = function(resp) 401L,
    .package = "httr2"
  )

  expect_true(xnatR:::is_auth_error(mock_resp))
})

test_that("is_auth_error identifies 403 status", {
  mock_resp <- structure(list(), class = "httr2_response")

  local_mocked_bindings(
    resp_status = function(resp) 403L,
    .package = "httr2"
  )

  expect_true(xnatR:::is_auth_error(mock_resp))
})

test_that("is_auth_error returns FALSE for other status codes", {
  mock_resp <- structure(list(), class = "httr2_response")

  local_mocked_bindings(
    resp_status = function(resp) 200L,
    .package = "httr2"
  )

  expect_false(xnatR:::is_auth_error(mock_resp))
})

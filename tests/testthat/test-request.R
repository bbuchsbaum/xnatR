test_that("require_auth aborts when not authenticated", {
  clear_test_auth()
  expect_error(xnatR:::require_auth(), class = "xnatR_auth_error")
})

test_that("require_auth succeeds when authenticated", {
  setup_test_auth()
  expect_silent(xnatR:::require_auth())
  clear_test_auth()
})

test_that("xnat_request builds correct request structure", {
  setup_test_auth("https://test.xnat.org")

  req <- xnatR:::xnat_request("data/projects")

  # Check request components
  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "test.xnat.org")
  expect_match(req$url, "data/projects")
  expect_true(!is.null(req$headers$Authorization))

  clear_test_auth()
})

test_that("xnat_request prefers JSESSION cookie auth when configured", {
  setup_test_auth("https://test.xnat.org")

  env <- get_xnatR_env()
  env$jsession <- "abc123"

  req <- xnatR:::xnat_request("data/projects")

  expect_true(!is.null(req$headers$Cookie))
  expect_match(req$headers$Cookie, "JSESSIONID=abc123")
  expect_true(is.null(req$headers$Authorization))

  clear_test_auth()
})

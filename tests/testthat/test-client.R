# Test explicit client workflow

test_that("xnat_request accepts an explicit client", {
  skip_if_not_installed("xnatR")
  client <- xnat_client(
    base_url = "https://test.xnat.org",
    username = "u",
    password = "p"
  )

  req <- xnatR:::xnat_request("data/projects", client = client)

  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "test.xnat.org")
  expect_match(req$url, "data/projects")
  expect_true(!is.null(req$headers$Authorization))
})

test_that("xnat_request uses cookie auth for client jsession", {
  skip_if_not_installed("xnatR")
  client <- xnat_client(
    base_url = "https://test.xnat.org",
    username = "u",
    password = "p",
    jsession = "abc123"
  )

  req <- xnatR:::xnat_request("data/projects", client = client)

  expect_true(!is.null(req$headers$Cookie))
  expect_match(req$headers$Cookie, "JSESSIONID=abc123")
  expect_true(is.null(req$headers$Authorization))
})

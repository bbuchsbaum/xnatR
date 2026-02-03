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

test_that("xnat_client creates object with correct structure", {
  client <- xnat_client(
    base_url = "https://test.xnat.org",
    username = "myuser",
    password = "mypass",
    ssl_verify = FALSE
  )

  expect_s3_class(client, "xnat_client")
  expect_equal(client$base_url, "https://test.xnat.org")
  expect_equal(client$username, "myuser")
  expect_equal(client$password, "mypass")
  expect_false(client$ssl_verify)
  expect_null(client$jsession)
})

test_that("xnat_client removes trailing slash from URL", {
  client <- xnat_client(
    base_url = "https://test.xnat.org///"
  )

  expect_equal(client$base_url, "https://test.xnat.org")
})

test_that("xnat_client validates base_url is required", {
  expect_error(xnat_client(base_url = NULL), "must be a non-empty character string")
  expect_error(xnat_client(base_url = ""), "must be a non-empty character string")
})

test_that("print.xnat_client outputs client info", {
  client <- xnat_client(
    base_url = "https://test.xnat.org",
    username = "testuser",
    password = "pass"
  )

  # Print should not error
  expect_no_error(capture.output(print(client), type = "output"))
})

test_that("xnat_connect validates base_url", {
  # Clear any env vars and mock config loading
  local_mocked_bindings(
    load_config = function() stop("Config file not found"),
    load_netrc = function(base_url) stop("No .netrc file"),
    .package = "xnatR"
  )

  withr::with_envvar(
    c(XNATR_HOST = NA, XNATR_USER = NA, XNATR_PASS = NA),
    {
      expect_error(
        xnat_connect(username = "user", password = "pass", verify = FALSE),
        "No XNAT server URL provided"
      )
    }
  )
})

test_that("xnat_connect creates client with credentials", {
  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      list(ResultSet = list(Result = list()))
    },
    .package = "xnatR"
  )

  client <- xnat_connect(
    base_url = "https://test.xnat.org",
    username = "testuser",
    password = "testpass",
    verify = TRUE
  )

  expect_s3_class(client, "xnat_client")
  expect_equal(client$base_url, "https://test.xnat.org")
  expect_equal(client$username, "testuser")
})

test_that("xnat_connect with verify=FALSE skips HTTP request", {
  http_called <- FALSE

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      http_called <<- TRUE
      list(ResultSet = list(Result = list()))
    },
    .package = "xnatR"
  )

  client <- xnat_connect(
    base_url = "https://test.xnat.org",
    username = "user",
    password = "pass",
    verify = FALSE
  )

  expect_false(http_called)
  expect_s3_class(client, "xnat_client")
})

test_that("xnat_current_client returns NULL when not authenticated", {
  clear_test_auth()
  expect_null(xnat_current_client())
})

test_that("xnat_current_client returns client when authenticated", {
  setup_test_auth("https://current.xnat.org")

  client <- xnat_current_client()

  expect_s3_class(client, "xnat_client")
  expect_equal(client$base_url, "https://current.xnat.org")
  expect_equal(client$username, "test_user")

  clear_test_auth()
})

test_that("with_xnat_client temporarily changes global session", {
  setup_test_auth("https://original.xnat.org")

  temp_client <- xnat_client(
    base_url = "https://temp.xnat.org",
    username = "temp_user",
    password = "temp_pass"
  )

  # Before - should be original
  expect_equal(xnat_server(), "https://original.xnat.org")

  # Inside with_xnat_client - should be temp
  with_xnat_client(temp_client, {
    expect_equal(xnat_server(), "https://temp.xnat.org")
    expect_equal(xnat_username(), "temp_user")
  })

  # After - should be restored to original
  expect_equal(xnat_server(), "https://original.xnat.org")
  expect_equal(xnat_username(), "test_user")

  clear_test_auth()
})

test_that("with_xnat_client restores NULL state", {
  clear_test_auth()

  temp_client <- xnat_client(
    base_url = "https://temp.xnat.org",
    username = "temp_user",
    password = "temp_pass"
  )

  expect_null(xnat_server())

  with_xnat_client(temp_client, {
    expect_equal(xnat_server(), "https://temp.xnat.org")
  })

  # Should restore to NULL state
  expect_null(xnat_server())
})

test_that("with_xnat_client validates client argument", {
  expect_error(
    with_xnat_client("not a client", { 1 + 1 }),
    "must be an xnat_client"
  )
})

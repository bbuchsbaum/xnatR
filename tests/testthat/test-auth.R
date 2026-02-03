test_that("is_authenticated returns FALSE when not authenticated", {
  clear_test_auth()
  expect_false(is_authenticated())
})

test_that("is_authenticated returns TRUE when credentials are set", {
  setup_test_auth()
  expect_true(is_authenticated())
  clear_test_auth()
})

test_that("xnat_server returns NULL when not authenticated", {
  clear_test_auth()
  expect_null(xnat_server())
})

test_that("xnat_server returns base_url when authenticated", {
  setup_test_auth("https://example.xnat.org")
  expect_equal(xnat_server(), "https://example.xnat.org")
  clear_test_auth()
})

test_that("resolve_credentials uses explicit arguments first", {
  withr::with_envvar(
    c(XNATR_HOST = "https://env.xnat.org",
      XNATR_USER = "env_user",
      XNATR_PASS = "env_pass"),
    {
      creds <- xnatR:::resolve_credentials(
        base_url = "https://explicit.xnat.org",
        username = "explicit_user",
        password = "explicit_pass"
      )
      expect_equal(creds$base_url, "https://explicit.xnat.org")
      expect_equal(creds$username, "explicit_user")
      expect_equal(creds$password, "explicit_pass")
    }
  )
})

test_that("resolve_credentials falls back to environment variables", {
  withr::with_envvar(
    c(XNATR_HOST = "https://env.xnat.org",
      XNATR_USER = "env_user",
      XNATR_PASS = "env_pass"),
    {
      creds <- xnatR:::resolve_credentials()
      expect_equal(creds$base_url, "https://env.xnat.org")
      expect_equal(creds$username, "env_user")
      expect_equal(creds$password, "env_pass")
    }
  )
})

test_that("xnat_logout clears credentials", {
  setup_test_auth()
  expect_true(is_authenticated())

  # Suppress the cli output during test
  suppressMessages(xnat_logout())

  expect_false(is_authenticated())
  expect_null(xnat_server())
})

test_that("authenticate_xnat validates required credentials", {
  # Clear any env vars that might provide credentials
  # Also mock load_config to prevent reading from config file
  local_mocked_bindings(
    load_config = function() stop("Config file not found"),
    load_netrc = function(base_url) stop("No .netrc file"),
    .package = "xnatR"
  )

  withr::with_envvar(
    c(XNATR_HOST = NA, XNATR_USER = NA, XNATR_PASS = NA),
    {
      # Missing base_url
      expect_error(
        authenticate_xnat(username = "user", password = "pass", verify = FALSE),
        "No XNAT server URL provided"
      )

      # Missing username
      expect_error(
        authenticate_xnat(base_url = "https://test.xnat.org", password = "pass", verify = FALSE),
        "No username provided"
      )

      # Missing password
      expect_error(
        authenticate_xnat(base_url = "https://test.xnat.org", username = "user", verify = FALSE),
        "No password provided"
      )
    }
  )
})

test_that("authenticate_xnat stores credentials in package environment", {
  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      list(ResultSet = list(Result = list()))
    },
    .package = "xnatR"
  )

  authenticate_xnat(
    base_url = "https://test.xnat.org",
    username = "test_user",
    password = "test_pass",
    verify = TRUE
  )

  env <- get_xnatR_env()
  expect_equal(env$base_url, "https://test.xnat.org")
  expect_equal(env$username, "test_user")
  expect_equal(env$password, "test_pass")
  expect_true(env$ssl_verify)

  clear_test_auth()
})

test_that("authenticate_xnat removes trailing slash from URL", {
  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      list(ResultSet = list(Result = list()))
    },
    .package = "xnatR"
  )

  authenticate_xnat(
    base_url = "https://test.xnat.org///",
    username = "user",
    password = "pass",
    verify = TRUE
  )

  env <- get_xnatR_env()
  expect_equal(env$base_url, "https://test.xnat.org")

  clear_test_auth()
})

test_that("authenticate_xnat with verify=FALSE does not make HTTP request", {
  http_called <- FALSE

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      http_called <<- TRUE
      list(ResultSet = list(Result = list()))
    },
    .package = "xnatR"
  )

  authenticate_xnat(
    base_url = "https://test.xnat.org",
    username = "user",
    password = "pass",
    verify = FALSE
  )

  expect_false(http_called)

  clear_test_auth()
})

test_that("xnat_username returns current username", {
  setup_test_auth()

  expect_equal(xnat_username(), "test_user")

  clear_test_auth()
  expect_null(xnat_username())
})

test_that("is_authenticated with xnat_client checks client credentials", {
  client <- list(
    base_url = "https://test.xnat.org",
    username = "user",
    password = "pass",
    jsession = NULL,
    ssl_verify = TRUE
  )
  class(client) <- "xnat_client"

  expect_true(is_authenticated(client))

  # Client with jsession instead of user/pass
  client2 <- list(
    base_url = "https://test.xnat.org",
    username = NULL,
    password = NULL,
    jsession = "abc123",
    ssl_verify = TRUE
  )
  class(client2) <- "xnat_client"

  expect_true(is_authenticated(client2))

  # Client with missing credentials
  client3 <- list(
    base_url = "https://test.xnat.org",
    username = NULL,
    password = NULL,
    jsession = NULL,
    ssl_verify = TRUE
  )
  class(client3) <- "xnat_client"

  expect_false(is_authenticated(client3))
})

test_that("xnat_server and xnat_username work with client", {
  client <- list(
    base_url = "https://client.xnat.org",
    username = "client_user",
    password = "pass",
    jsession = NULL,
    ssl_verify = TRUE
  )
  class(client) <- "xnat_client"

  expect_equal(xnat_server(client), "https://client.xnat.org")
  expect_equal(xnat_username(client), "client_user")
})

test_that("xnat_token_issue returns token structure", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(path, "data/services/tokens/issue")
      list(
        alias = "test_alias_123",
        secret = "secret_abc",
        estimatedExpirationTime = "2024-12-31T23:59:59"
      )
    },
    .package = "xnatR"
  )

  token <- xnat_token_issue()

  expect_equal(token$alias, "test_alias_123")
  expect_equal(token$secret, "secret_abc")
  expect_equal(token$estimatedExpirationTime, "2024-12-31T23:59:59")

  clear_test_auth()
})

test_that("xnat_token_validate returns valid result", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_match(path, "data/services/tokens/validate/my_alias/my_secret")
      list(
        alias = "my_alias",
        estimatedExpirationTime = "2024-12-31T23:59:59"
      )
    },
    .package = "xnatR"
  )

  result <- xnat_token_validate("my_alias", "my_secret")

  expect_true(result$valid)
  expect_equal(result$alias, "my_alias")

  clear_test_auth()
})

test_that("xnat_token_validate returns invalid for error", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      stop("Token not found")
    },
    .package = "xnatR"
  )

  result <- xnat_token_validate("bad_alias", "bad_secret")

  expect_false(result$valid)

  clear_test_auth()
})

test_that("xnat_token_invalidate makes correct request", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_post = function(path, body = NULL, query = NULL, client = NULL) {
      expect_match(path, "data/services/tokens/invalidate/my_alias/my_secret")
      NULL
    },
    .package = "xnatR"
  )

  expect_true(xnat_token_invalidate("my_alias", "my_secret"))

  clear_test_auth()
})

test_that("xnat_token_list returns tibble", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(path, "data/services/tokens/list")
      data.frame(
        alias = c("token1", "token2"),
        xdatUserId = c("user1", "user1"),
        estimatedExpirationTime = c("2024-12-31", "2024-12-31"),
        stringsAsFactors = FALSE
      )
    },
    .package = "xnatR"
  )

  tokens <- xnat_token_list()

  expect_s3_class(tokens, "tbl_df")
  expect_equal(nrow(tokens), 2)
  expect_true("alias" %in% names(tokens))

  clear_test_auth()
})

test_that("xnat_token_list returns empty tibble when no tokens", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      list()
    },
    .package = "xnatR"
  )

  tokens <- xnat_token_list()

  expect_s3_class(tokens, "tbl_df")
  expect_equal(nrow(tokens), 0)

  clear_test_auth()
})

test_that("load_netrc parses credentials correctly", {
  # Create temporary netrc file
  netrc_content <- "machine test.xnat.org login myuser password mypass"
  temp_netrc <- tempfile()
  writeLines(netrc_content, temp_netrc)

  local_mocked_bindings(
    path.expand = function(path) {
      if (path == "~/.netrc") {
        temp_netrc
      } else {
        base::path.expand(path)
      }
    },
    .package = "base"
  )

  creds <- xnatR:::load_netrc("https://test.xnat.org")

  expect_equal(creds$username, "myuser")
  expect_equal(creds$password, "mypass")

  file.remove(temp_netrc)
})

test_that("load_netrc errors when file not found", {
  local_mocked_bindings(
    path.expand = function(path) {
      if (path == "~/.netrc") {
        "/nonexistent/.netrc"
      } else {
        base::path.expand(path)
      }
    },
    .package = "base"
  )

  expect_error(xnatR:::load_netrc("https://test.xnat.org"), ".netrc file not found")
})

test_that("load_netrc errors when host not found", {
  netrc_content <- "machine other.xnat.org login myuser password mypass"
  temp_netrc <- tempfile()
  writeLines(netrc_content, temp_netrc)

  local_mocked_bindings(
    path.expand = function(path) {
      if (path == "~/.netrc") {
        temp_netrc
      } else {
        base::path.expand(path)
      }
    },
    .package = "base"
  )

  expect_error(xnatR:::load_netrc("https://test.xnat.org"), "No matching entry")

  file.remove(temp_netrc)
})

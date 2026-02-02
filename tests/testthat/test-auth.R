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

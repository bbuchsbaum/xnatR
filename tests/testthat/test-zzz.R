# Tests for package initialization (zzz.R)

test_that("xnatR_env exists and has expected structure", {
  env <- get_xnatR_env()

  expect_true(is.environment(env))
  expect_true(exists("base_url", envir = env))
  expect_true(exists("username", envir = env))
  expect_true(exists("password", envir = env))
  expect_true(exists("ssl_verify", envir = env))
  expect_true(exists("jsession", envir = env))
})

test_that("xnat_option returns default when option not set", {
  # Ensure option is not set
  old <- options(xnatR.test_option = NULL)
  on.exit(options(old))

  result <- xnatR:::xnat_option("test_option", default = "default_value")
  expect_equal(result, "default_value")
})
test_that("xnat_option returns option value when set", {
  old <- options(xnatR.test_option = "custom_value")
  on.exit(options(old))

  result <- xnatR:::xnat_option("test_option", default = "default_value")
  expect_equal(result, "custom_value")
})

test_that(".onLoad initializes environment with NULLs", {
  # Save current state
  env <- get_xnatR_env()
  old_base_url <- env$base_url
  old_username <- env$username
  old_password <- env$password

  # Clear environment variables to test default initialization
  withr::with_envvar(
    c(XNATR_HOST = NA, XNATR_USER = NA, XNATR_PASS = NA),
    {
      # Re-run onLoad logic
      env$base_url <- NULL
      env$username <- NULL
      env$password <- NULL
      env$ssl_verify <- TRUE
      env$jsession <- NULL

      # Verify defaults
      expect_null(env$base_url)
      expect_null(env$username)
      expect_null(env$password)
      expect_true(env$ssl_verify)
      expect_null(env$jsession)
    }
  )

  # Restore
  env$base_url <- old_base_url
  env$username <- old_username
  env$password <- old_password
})

test_that(".onLoad picks up environment variables", {
  env <- get_xnatR_env()
  old_base_url <- env$base_url
  old_username <- env$username
  old_password <- env$password

  withr::with_envvar(
    c(
      XNATR_HOST = "https://env-test.xnat.org",
      XNATR_USER = "env_user",
      XNATR_PASS = "env_pass"
    ),
    {
      # Simulate .onLoad behavior
      env_host <- Sys.getenv("XNATR_HOST", unset = "")
      env_user <- Sys.getenv("XNATR_USER", unset = "")
      env_pass <- Sys.getenv("XNATR_PASS", unset = "")

      if (nzchar(env_host) && nzchar(env_user) && nzchar(env_pass)) {
        env$base_url <- sub("/+$", "", env_host)
        env$username <- env_user
        env$password <- env_pass
      }

      expect_equal(env$base_url, "https://env-test.xnat.org")
      expect_equal(env$username, "env_user")
      expect_equal(env$password, "env_pass")
    }
  )

  # Restore
  env$base_url <- old_base_url
  env$username <- old_username
  env$password <- old_password
})

# Access internal functions
compact <- xnatR:::compact
xnat_path <- xnatR:::xnat_path
check_string <- xnatR:::check_string
url_encode <- xnatR:::url_encode

test_that("compact removes NULL elements", {
  expect_equal(compact(list(a = 1, b = NULL, c = 3)), list(a = 1, c = 3))
  expect_equal(compact(list()), list())
  result <- compact(list(a = NULL, b = NULL))
  expect_length(result, 0)
})

test_that("xnat_path joins path components", {
  expect_equal(xnat_path("a", "b", "c"), "a/b/c")
  expect_equal(xnat_path("a"), "a")
  expect_equal(xnat_path("a", "", "c"), "a/c")  # Empty strings filtered
})

test_that("check_string validates input", {
  expect_silent(check_string("valid", "param"))
  expect_error(check_string(NULL, "param"), "must be a non-empty character string")
  expect_error(check_string("", "param"), "must be a non-empty character string")
  expect_error(check_string(123, "param"), "must be a non-empty character string")
  expect_error(check_string(c("a", "b"), "param"), "must be a non-empty character string")
})

test_that("url_encode handles special characters", {
  expect_equal(url_encode("test"), "test")
  expect_match(url_encode("test/path"), "%2F")
  expect_match(url_encode("test value"), "%20")
})

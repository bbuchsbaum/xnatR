# Access internal functions
compact <- xnatR:::compact
xnat_path <- xnatR:::xnat_path
check_string <- xnatR:::check_string
url_encode <- xnatR:::url_encode
parse_xnat_result <- xnatR:::parse_xnat_result

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

test_that("parse_xnat_result uses ResultSet Columns when Result names are empty", {
  result_df <- data.frame(
    matrix(c("PROJECT_1", "Project One", "PROJECT_2", "Project Two"), ncol = 2, byrow = TRUE),
    stringsAsFactors = FALSE
  )
  names(result_df) <- c("", "")

  json <- list(
    ResultSet = list(
      Columns = c("ID", "name"),
      Result = result_df
    )
  )

  parsed <- parse_xnat_result(json)
  expect_equal(names(parsed), c("ID", "name"))
  expect_equal(parsed$ID, c("PROJECT_1", "PROJECT_2"))
  expect_equal(parsed$name, c("Project One", "Project Two"))
})

test_that("parse_xnat_result keeps working without ResultSet Columns", {
  result_df <- data.frame(
    matrix(c("A", "B", "C", "D"), ncol = 2, byrow = TRUE),
    stringsAsFactors = FALSE
  )
  names(result_df) <- c("", "")

  json <- list(ResultSet = list(Result = result_df))
  parsed <- parse_xnat_result(json)

  expect_equal(nrow(parsed), 2)
  expect_false(any(names(parsed) == ""))
})

test_that("parse_xnat_result infers column names from nested cell names", {
  json <- list(
    ResultSet = list(
      Result = list(
        list(list(ID = "PROJECT_1"), list(ID = "PROJECT_2")),
        list(list(name = "Project One"), list(name = "Project Two")),
        list(list(description = "D1"), list(description = "D2"))
      )
    )
  )

  parsed <- parse_xnat_result(json)
  expect_equal(names(parsed), c("ID", "name", "description"))
  expect_equal(parsed$ID, c("PROJECT_1", "PROJECT_2"))
  expect_equal(parsed$name, c("Project One", "Project Two"))
  expect_equal(parsed$description, c("D1", "D2"))
})

test_that("parse_xnat_result handles list-of-records shape without transposing", {
  json <- list(
    ResultSet = list(
      Result = list(
        list(ID = "PROJECT_1", name = "Project One", URI = "/data/projects/PROJECT_1"),
        list(ID = "PROJECT_2", name = "Project Two", URI = "/data/projects/PROJECT_2")
      )
    )
  )

  parsed <- parse_xnat_result(json)
  expect_equal(nrow(parsed), 2)
  expect_equal(names(parsed), c("ID", "name", "URI"))
  expect_equal(parsed$ID, c("PROJECT_1", "PROJECT_2"))
  expect_equal(parsed$name, c("Project One", "Project Two"))
})

test_that("parse_xnat_result handles list-of-records with varying key order", {
  json <- list(
    ResultSet = list(
      Result = list(
        list(secondary_ID = "SEC_1", ID = "PROJECT_1", name = "Project One", URI = "/data/projects/PROJECT_1"),
        list(name = "Project Two", URI = "/data/projects/PROJECT_2", ID = "PROJECT_2", secondary_ID = "SEC_2")
      )
    )
  )

  parsed <- parse_xnat_result(json)
  expect_equal(nrow(parsed), 2)
  expect_true(all(c("ID", "name", "secondary_ID", "URI") %in% names(parsed)))
  expect_equal(parsed$ID, c("PROJECT_1", "PROJECT_2"))
  expect_equal(parsed$secondary_ID, c("SEC_1", "SEC_2"))
})

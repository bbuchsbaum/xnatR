# Internal browse parsing utilities

test_that("browse selection parser supports singles, lists, ranges", {
  skip_if_not_installed("xnatR")
  parse <- xnatR:::.browse_parse_selection

  expect_equal(parse("1", 5), 1L)
  expect_equal(parse("1,3,5", 5), c(1L, 3L, 5L))
  expect_equal(parse("2-4", 5), c(2L, 3L, 4L))
  expect_equal(parse(" 2 - 4 , 1 ", 5), c(1L, 2L, 3L, 4L))
  expect_equal(parse("all", 3), c(1L, 2L, 3L))
})

test_that("browse selection parser rejects invalid specs", {
  skip_if_not_installed("xnatR")
  parse <- xnatR:::.browse_parse_selection

  expect_error(parse("0", 5))
  expect_error(parse("6", 5))
  expect_error(parse("4-2", 5))
  expect_error(parse("a", 5))
  expect_error(parse("1-a", 5))
})

test_that("browse loop supports selection, filtering, and quit", {
  skip_if_not_installed("xnatR")

  fetch_page <- function(limit, offset) {
    tibble::tibble(ID = c("A", "B", "C"), name = c("alpha", "bravo", "charlie"))
  }

  inputs <- c("2")
  cursor <- 0L
  input_fn <- function(prompt) {
    cursor <<- cursor + 1L
    inputs[[cursor]]
  }

  selected <- NULL
  invisible(capture.output({
    selected <- xnatR:::.browse_loop(
      fetch_page = fetch_page,
      title = "Test",
      columns = c("ID", "name"),
      page_size = 20,
      input_fn = input_fn
    )
  }))

  expect_s3_class(selected, "tbl_df")
  expect_equal(selected$ID, "B")

  inputs <- c("/br", "1")
  cursor <- 0L
  input_fn <- function(prompt) {
    cursor <<- cursor + 1L
    inputs[[cursor]]
  }

  selected_filtered <- NULL
  invisible(capture.output({
    selected_filtered <- xnatR:::.browse_loop(
      fetch_page = fetch_page,
      title = "Test",
      columns = c("ID", "name"),
      page_size = 20,
      input_fn = input_fn
    )
  }))

  expect_equal(selected_filtered$ID, "B")

  inputs <- c("q")
  cursor <- 0L
  input_fn <- function(prompt) {
    cursor <<- cursor + 1L
    inputs[[cursor]]
  }

  quit_value <- NULL
  invisible(capture.output({
    quit_value <- xnatR:::.browse_loop(
      fetch_page = fetch_page,
      title = "Test",
      columns = c("ID", "name"),
      page_size = 20,
      input_fn = input_fn
    )
  }))
  expect_null(quit_value)
})

test_that("browse wrappers call list functions in non-interactive mode", {
  skip_if_not_installed("xnatR")

  called <- list()
  testthat::local_mocked_bindings(
    list_projects = function(columns = NULL, limit = NULL, offset = NULL, client = NULL) {
      called$list_projects <<- list(columns = columns, limit = limit, offset = offset, client = client)
      tibble::tibble(ID = "P1", name = "Project 1")
    },
    list_subjects = function(project_id, columns = NULL, limit = NULL, offset = NULL, client = NULL) {
      called$list_subjects <<- list(project_id = project_id, columns = columns, limit = limit, offset = offset, client = client)
      tibble::tibble(ID = "S1", label = "Subject 1")
    },
    .package = "xnatR"
  )

  fake_client <- xnatR::xnat_client(base_url = "https://example.org", username = "u", password = "p")

  projects <- xnatR::xnat_browse_projects(client = fake_client, columns = c("ID", "name"), .interactive = FALSE)
  expect_equal(projects$ID, "P1")
  expect_true(!is.null(called$list_projects))
  expect_equal(called$list_projects$columns, c("ID", "name"))
  expect_s3_class(called$list_projects$client, "xnat_client")

  subjects <- xnatR::xnat_browse_subjects(project_id = "P1", client = fake_client, columns = c("ID", "label"), .interactive = FALSE)
  expect_equal(subjects$ID, "S1")
  expect_equal(called$list_subjects$project_id, "P1")
  expect_equal(called$list_subjects$columns, c("ID", "label"))
  expect_s3_class(called$list_subjects$client, "xnat_client")
})

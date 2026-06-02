test_that("CLI help returns success", {
  out <- capture.output(status <- xnatR_cli("--help"))

  expect_equal(status, 0L)
  expect_true(any(grepl("Usage: xnatR", out, fixed = TRUE)))
})

test_that("CLI tolerates Rscript argument separator", {
  out <- capture.output(status <- xnatR_cli(c("--", "--help")))

  expect_equal(status, 0L)
  expect_true(any(grepl("Usage: xnatR", out, fixed = TRUE)))
})

test_that("CLI command help returns success", {
  out <- capture.output(status <- xnatR_cli(c("help", "list-subjects")))

  expect_equal(status, 0L)
  expect_true(any(grepl("--project PROJECT", out, fixed = TRUE)))
})

test_that("CLI rejects unknown options", {
  out <- capture.output(status <- xnatR_cli(c("list-projects", "--bogus")), type = "message")

  expect_equal(status, 2L)
  expect_true(any(grepl("Unknown option", out, fixed = TRUE)))
})

test_that("CLI rejects missing option values", {
  out <- capture.output(status <- xnatR_cli(c("list-projects", "--limit")), type = "message")

  expect_equal(status, 2L)
  expect_true(any(grepl("Missing value for --limit", out, fixed = TRUE)))
})

test_that("CLI accepts --key=value and writes JSON", {
  local_mocked_bindings(
    list_projects = function(columns = NULL, limit = NULL, offset = NULL, client = NULL) {
      expect_equal(columns, c("ID", "name"))
      expect_equal(limit, 2L)
      expect_equal(client$base_url, "https://test.xnat.org")
      mock_projects(2)
    },
    .package = "xnatR"
  )

  out <- capture.output(status <- xnatR_cli(c(
    "list-projects",
    "--host=https://test.xnat.org",
    "--user", "test_user",
    "--password", "test_pass",
    "--columns=ID,name",
    "--limit=2",
    "--json"
  )))

  expect_equal(status, 0L)
  json <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_equal(nrow(json), 2)
  expect_equal(names(json), names(mock_projects(2)))
})

test_that("CLI dispatches required hierarchy options", {
  local_mocked_bindings(
    list_scans = function(project_id, subject_id, experiment_id, columns = NULL, limit = NULL, offset = NULL, client = NULL) {
      expect_equal(project_id, "P1")
      expect_equal(subject_id, "S1")
      expect_equal(experiment_id, "E1")
      expect_equal(limit, 10L)
      mock_scans(1)
    },
    .package = "xnatR"
  )

  out <- capture.output(status <- xnatR_cli(c(
    "list-scans",
    "--host", "https://test.xnat.org",
    "--user", "test_user",
    "--password", "test_pass",
    "--project", "P1",
    "--subject", "S1",
    "--experiment", "E1",
    "--limit", "10"
  )))

  expect_equal(status, 0L)
  expect_true(any(grepl("Series 1", out, fixed = TRUE)))
})

test_that("CLI reports missing required command options", {
  out <- capture.output(status <- xnatR_cli(c(
    "list-subjects",
    "--host", "https://test.xnat.org",
    "--user", "test_user",
    "--password", "test_pass"
  )), type = "message")

  expect_equal(status, 2L)
  expect_true(any(grepl("Missing required option --project", out, fixed = TRUE)))
})

test_that("CLI download-experiment dispatches options and JSON path", {
  local_mocked_bindings(
    download_experiment = function(experiment_id, scan_id = "ALL", format = "zip", dest_dir = tempdir(), dest_file = NULL,
                                   extract = FALSE, progress = TRUE, strict = TRUE, client = NULL) {
      expect_equal(experiment_id, "E1")
      expect_equal(scan_id, "2")
      expect_equal(format, "tar.gz")
      expect_false(progress)
      expect_false(strict)
      file.path(dest_dir, "E1.tar.gz")
    },
    .package = "xnatR"
  )

  out <- capture.output(status <- xnatR_cli(c(
    "download-experiment",
    "--host", "https://test.xnat.org",
    "--user", "test_user",
    "--password", "test_pass",
    "--experiment", "E1",
    "--scan", "2",
    "--format", "tar.gz",
    "--dest-dir", tempdir(),
    "--no-progress",
    "--no-strict",
    "--json"
  )))

  expect_equal(status, 0L)
  json <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_match(json$path, "E1\\.tar\\.gz$")
})

test_that("install_cli copies wrapper and refuses overwrite", {
  dest_dir <- withr::local_tempdir()

  installed <- install_cli(dest_dir)
  expect_true(file.exists(installed[["xnatR"]]))

  expect_error(
    install_cli(dest_dir),
    "Command already exists",
    fixed = TRUE
  )

  installed_again <- install_cli(dest_dir, overwrite = TRUE)
  expect_equal(installed_again[["xnatR"]], installed[["xnatR"]])
})

test_that("exec wrapper smoke test returns help", {
  wrapper <- system.file("exec", "xnatR", package = "xnatR")
  skip_if_not(file.exists(wrapper))
  skip_if_not_installed("pkgload")

  result <- system2(file.path(R.home("bin"), "Rscript"), c(wrapper, "--help"), stdout = TRUE, stderr = TRUE)
  expect_equal(attr(result, "status") %||% 0L, 0L)
  expect_true(any(grepl("Usage: xnatR", result, fixed = TRUE)))
})

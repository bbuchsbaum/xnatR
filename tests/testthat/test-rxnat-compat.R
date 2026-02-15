# Tests for search_scans, list_experiment_files_all, download_experiment

test_that("search_scans builds XML criteria and normalizes columns", {
  setup_test_auth()

  captured_xml <- NULL
  local_mocked_bindings(
    execute_search_xml = function(xml, format = "json", conn = NULL) {
      captured_xml <<- xml
      tibble::tibble(
        SUBJECT_ID = "SUBJ_1",
        PROJECT = "PROJ_1",
        AGE = "30",
        `Session ID` = "EXP_1",
        VOXELRES_UNITS = "mm"
      )
    },
    .package = "xnatR"
  )

  out <- search_scans(subject_id = "SUBJ_1", project_id = "PROJ_1")

  expect_match(captured_xml, "xnat:mrSessionData.SUBJECT_ID", fixed = TRUE)
  expect_match(captured_xml, "<xdat:value>SUBJ_1</xdat:value>", fixed = TRUE)
  expect_match(captured_xml, "xnat:mrSessionData.PROJECT", fixed = TRUE)
  expect_match(captured_xml, "<xdat:value>PROJ_1</xdat:value>", fixed = TRUE)
  expect_true("subject_ID" %in% names(out))
  expect_true("Project" %in% names(out))
  expect_true("experiment_ID" %in% names(out))
  expect_true("Voxel_res" %in% names(out))

  clear_test_auth()
})

test_that("search_scans uses uniform filter names", {
  setup_test_auth()

  captured_xml <- NULL
  local_mocked_bindings(
    execute_search_xml = function(xml, format = "json", conn = NULL) {
      captured_xml <<- xml
      tibble::tibble(PROJECT = "PROJ_1")
    },
    .package = "xnatR"
  )

  out <- search_scans(project_id = "PROJ_1", scan_type = "T1")

  expect_match(captured_xml, "xnat:mrSessionData.PROJECT", fixed = TRUE)
  expect_match(captured_xml, "<xdat:value>PROJ_1</xdat:value>", fixed = TRUE)
  expect_match(captured_xml, "xnat:mrScanData.TYPE", fixed = TRUE)
  expect_match(captured_xml, "<xdat:value>T1</xdat:value>", fixed = TRUE)
  expect_true("Project" %in% names(out))

  clear_test_auth()
})

test_that("list_experiment_files_all falls back to scans/ALL/files when needed", {
  setup_test_auth()

  calls <- character()
  local_mocked_bindings(
    xnat_get_tibble = function(path, query = NULL, class_name = NULL, client = NULL) {
      calls <<- c(calls, path)
      if (length(calls) == 1L) {
        tibble::tibble()
      } else {
        tibble::tibble(Name = "file1.dcm", URI = "/data/files/file1.dcm")
      }
    },
    .package = "xnatR"
  )

  out <- list_experiment_files_all("EXP_123")

  expect_equal(calls[[1]], "data/experiments/EXP_123/files")
  expect_equal(calls[[2]], "data/experiments/EXP_123/scans/ALL/files")
  expect_equal(nrow(out), 1)
  expect_equal(attr(out, "experiment_id"), "EXP_123")

  clear_test_auth()
})

test_that("list_experiment_files_all can disable fallback", {
  setup_test_auth()

  calls <- character()
  local_mocked_bindings(
    xnat_get_tibble = function(path, query = NULL, class_name = NULL, client = NULL) {
      calls <<- c(calls, path)
      tibble::tibble()
    },
    .package = "xnatR"
  )

  out <- list_experiment_files_all("EXP_456", include_scan_level = FALSE)

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]], "data/experiments/EXP_456/files")
  expect_equal(nrow(out), 0)

  clear_test_auth()
})

test_that("download_experiment builds experiment-level archive path", {
  setup_test_auth()

  captured <- list()
  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      captured$path <<- path
      captured$dest_file <<- dest_file
      captured$query <<- query
      captured$progress <<- progress
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  out <- download_experiment(
    experiment_id = "EXP_1",
    scan_id = "T1",
    dest_dir = tempdir(),
    progress = TRUE,
    strict = TRUE
  )

  expect_equal(captured$path, "data/experiments/EXP_1/scans/T1/files")
  expect_equal(captured$query$format, "zip")
  expect_true(isTRUE(captured$progress))
  expect_match(basename(out), "^EXP_1\\.zip$")

  clear_test_auth()
})

test_that("download_experiment supports tar.gz and custom filename", {
  setup_test_auth()

  captured <- list()
  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      captured$path <<- path
      captured$dest_file <<- dest_file
      captured$query <<- query
      invisible(dest_file)
    },
    .package = "xnatR"
  )

  out <- download_experiment(
    experiment_id = "EXP_2",
    scan_id = "ALL",
    format = "tar.gz",
    dest_dir = tempdir(),
    dest_file = "custom.tar.gz",
    progress = FALSE
  )

  expect_equal(captured$path, "data/experiments/EXP_2/scans/ALL/files")
  expect_equal(captured$query$format, "tar.gz")
  expect_match(out, "custom\\.tar\\.gz$")

  clear_test_auth()
})

test_that("download_experiment returns NULL on error when strict=FALSE", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_download = function(path, dest_file, query = NULL, progress = TRUE, client = NULL) {
      stop("boom")
    },
    .package = "xnatR"
  )

  expect_null(
    suppressMessages(
      download_experiment(
        experiment_id = "EXP_FAIL",
        dest_dir = tempdir(),
        strict = FALSE
      )
    )
  )

  clear_test_auth()
})

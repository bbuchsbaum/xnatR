# Tests for dicom_dump

test_that("dicom_dump returns xnat_dicom_dump tibble for a whole session", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(path, "data/services/dicomdump")
      expect_equal(query$src, "/archive/experiments/E1")
      expect_null(query$field)
      mock_resultset(mock_dicom_dump())
    },
    .package = "xnatR"
  )

  result <- dicom_dump("E1")

  expect_s3_class(result, "xnat_dicom_dump")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_equal(attr(result, "experiment_id"), "E1")
  expect_null(attr(result, "scan_id"))
  expect_true(all(c("tag1", "vr", "value", "desc") %in% names(result)))

  clear_test_auth()
})

test_that("dicom_dump scopes src to a scan and explodes field tags", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$src, "/archive/experiments/E1/scans/1")
      expect_equal(query$field, c("00080060", "00180080"))
      mock_resultset(mock_dicom_dump()[1:2, ])
    },
    .package = "xnatR"
  )

  result <- dicom_dump("E1", scan_id = "1", fields = c("00080060", "00180080"))

  expect_equal(nrow(result), 2)
  expect_equal(attr(result, "scan_id"), "1")

  clear_test_auth()
})

test_that("dicom_dump normalises tag forms to 8-digit hex", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      expect_equal(query$field, c("00100010", "00180080", "00180081"))
      mock_resultset(mock_dicom_dump()[1, ])
    },
    .package = "xnatR"
  )

  dicom_dump("E1", fields = c("(0010,0010)", "0018,0080", "00180081"))

  clear_test_auth()
})

test_that("dicom_dump leaves non-tag fields (keywords) untouched", {
  expect_equal(
    xnatR:::.normalize_dicom_field(c("(0008,0060)", "PatientName", "00180080")),
    c("00080060", "PatientName", "00180080")
  )
})

test_that("dicom_dump validates required experiment_id", {
  setup_test_auth()

  expect_error(dicom_dump(NULL), "experiment_id")
  expect_error(dicom_dump(123), "experiment_id")

  clear_test_auth()
})

test_that("print.xnat_dicom_dump shows context without error", {
  setup_test_auth()

  local_mocked_bindings(
    xnat_get = function(path, query = NULL, client = NULL) {
      mock_resultset(mock_dicom_dump())
    },
    .package = "xnatR"
  )

  result <- dicom_dump("Exp", scan_id = "2")
  expect_no_error(capture.output(print(result), type = "output"))

  clear_test_auth()
})

test_that("CLI dicom-dump dispatches options", {
  local_mocked_bindings(
    dicom_dump = function(experiment_id, scan_id = NULL, fields = NULL, client = NULL) {
      expect_equal(experiment_id, "E1")
      expect_equal(scan_id, "1")
      expect_equal(fields, c("00080060", "00180080"))
      expect_equal(client$base_url, "https://test.xnat.org")
      mock_dicom_dump()
    },
    .package = "xnatR"
  )

  out <- capture.output(status <- xnatR_cli(c(
    "dicom-dump",
    "--host", "https://test.xnat.org",
    "--user", "test_user",
    "--password", "test_pass",
    "--experiment", "E1",
    "--scan", "1",
    "--fields", "00080060,00180080"
  )))

  expect_equal(status, 0L)
  expect_true(any(grepl("Modality", out, fixed = TRUE)))
})

test_that("CLI dicom-dump requires --experiment", {
  out <- capture.output(status <- xnatR_cli(c(
    "dicom-dump",
    "--host", "https://test.xnat.org",
    "--user", "test_user",
    "--password", "test_pass"
  )), type = "message")

  expect_equal(status, 2L)
  expect_true(any(grepl("Missing required option --experiment", out, fixed = TRUE)))
})

test_that("CLI dicom-dump help is available", {
  out <- capture.output(status <- xnatR_cli(c("help", "dicom-dump")))

  expect_equal(status, 0L)
  expect_true(any(grepl("xnatR dicom-dump --experiment", out, fixed = TRUE)))
})

# Test list functions parameter validation

test_that("list_subjects requires project_id", {
  setup_test_auth()
  expect_error(list_subjects(), "project_id")
  expect_error(list_subjects(project_id = ""), "must be a non-empty")
  expect_error(list_subjects(project_id = NULL), "must be a non-empty")
  clear_test_auth()
})

test_that("list_experiments requires project_id and subject_id", {
  setup_test_auth()
  expect_error(list_experiments(), "project_id")
  expect_error(list_experiments(project_id = "P"), "subject_id")
  clear_test_auth()
})

test_that("list_scans requires project_id, subject_id, and experiment_id", {
  setup_test_auth()
  expect_error(list_scans(), "project_id")
  expect_error(list_scans(project_id = "P"), "subject_id")
  expect_error(list_scans(project_id = "P", subject_id = "S"), "experiment_id")
  clear_test_auth()
})

test_that("list_resources requires all path components", {
  setup_test_auth()
  expect_error(list_resources(), "project_id")
  expect_error(list_resources(project_id = "P"), "subject_id")
  expect_error(list_resources(project_id = "P", subject_id = "S"), "experiment_id")
  expect_error(
    list_resources(project_id = "P", subject_id = "S", experiment_id = "E"),
    "scan_id"
  )
  clear_test_auth()
})

test_that("list_files requires all path components including resource", {
  setup_test_auth()
  expect_error(
    list_files(project_id = "P", subject_id = "S",
               experiment_id = "E", scan_id = "1"),
    "resource"
  )
  clear_test_auth()
})

test_that("list_assessors requires all path components", {
  setup_test_auth()
  expect_error(list_assessors(), "project_id")
  expect_error(list_assessors(project_id = "P"), "subject_id")
  expect_error(list_assessors(project_id = "P", subject_id = "S"), "experiment_id")
  clear_test_auth()
})

test_that("list_queryable_fields requires xsi_type", {
  setup_test_auth()
  expect_error(list_queryable_fields(), "xsi_type")
  expect_error(list_queryable_fields(xsi_type = ""), "must be a non-empty")
  clear_test_auth()
})

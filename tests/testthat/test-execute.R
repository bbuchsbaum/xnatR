executor_fixture <- function(code, backend = "curl") {
  skip_if(Sys.which("python3") == "" || Sys.which("unzip") == "")
  scratch <- tempfile()
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  fixture <- file.path(scratch, "fixture.json")
  ready <- file.path(scratch, "port")
  control <- file.path(scratch, "control.json")
  writeLines(plan_json(inventory_fixture()), fixture)
  writeLines('{}', control)
  server <- start_fixture_server("inventory-server.py", c(fixture, "legacy", control), ready)
  on.exit(server$proc$kill(), add = TRUE)
  client <- xnat_client(server$url, "u", "p", backend = backend)
  inv <- xnat_inventory(project_ids = "P", client = client)
  plan <- xnat_plan_download(inv, c("E1", "E2"), file.path(scratch, "archives"), client = client)
  downloads <- function() if (file.exists(paste0(control, ".downloads"))) readLines(paste0(control, ".downloads")) else character()
  code(plan, client, control, downloads)
}

test_that("both backends restart truncated transfers without re-fetching verified resources", {
  for (backend in c("httr2", "curl")) executor_fixture(function(plan, client, control, downloads) {
    writeLines('{"mode":"truncate","target":"E2"}', control)
    first <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_false(first$complete)
    expect_equal(first$resources$status, c("downloaded", "failed"))
    expect_true(file.exists(first$resources$partial_path[2]))
    expect_false(file.exists(plan$resources$destination[2]))
    expect_equal(first$verified_files, 2L)
    writeLines('{}', control)
    second <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(second$complete)
    expect_equal(second$resources$status, c("reused", "downloaded"))
    expect_equal(sum(grepl("E1", downloads())), 1L)
    expect_equal(sum(grepl("E2", downloads())), 2L)
    third <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(third$complete)
    expect_equal(third$resources$status, c("reused", "reused"))
    expect_length(downloads(), 3L)
    expect_true(jsonlite::fromJSON(third$summary_path)$complete)
    expect_equal(third$verified_files, plan$expected_files)
  }, backend)
})

test_that("CRC failures, extra members and unsafe paths never publish receipts", {
  for (mode in c("crc", "extra", "unsafe")) executor_fixture(function(plan, client, control, downloads) {
    writeLines(plan_json(list(mode = mode)), control)
    result <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_false(result$complete)
    expect_equal(result$resources$status, c("failed", "pending"))
    expect_equal(result$resources$error_class[1], "xnatR_verification_error")
    expect_false(any(file.exists(plan$resources$destination)))
    expect_false(any(file.exists(result$resources$receipt_path)))
    expect_true(file.exists(result$resources$partial_path[1]))
  })
})

test_that("changed sources refuse reuse and invalidate prior whole-plan completion", {
  executor_fixture(function(plan, client, control, downloads) {
    first <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(first$complete)
    changed <- inventory_fixture()[[plan$resources$path[1]]]
    changed$Size[1] <- "11"
    writeLines(plan_json(list(catalog_changes = stats::setNames(list(changed), plan$resources$path[1]))), control)
    next_run <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_false(next_run$complete)
    expect_equal(next_run$error_class, "xnatR_source_changed")
    expect_length(downloads(), 2L)
    expect_false(jsonlite::fromJSON(next_run$summary_path)$complete)
    expect_true(all(file.exists(plan$resources$destination)))
  })
})

test_that("corrupt archives and stale receipts are preserved and downloaded again", {
  executor_fixture(function(plan, client, control, downloads) {
    first <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(first$complete)
    writeBin(charToRaw("corrupt"), plan$resources$destination[1])
    writeLines('{}', paste0(plan$resources$destination[2], ".receipt.json"))
    second <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(second$complete)
    expect_equal(second$resources$status, c("downloaded", "downloaded"))
    expect_length(downloads(), 4L)
    retained <- list.files(plan$destination_root, recursive = TRUE, pattern = "unverified-")
    expect_length(retained, 4L)
  })
})

test_that("user interruption records incompleteness and permits restart", {
  executor_fixture(function(plan, client, control, downloads) {
    download <- execution_download
    calls <- 0L
    local_mocked_bindings(execution_download = function(...) {
      calls <<- calls + 1L
      if (calls == 2L) stop(structure(list(message = "synthetic interrupt"), class = c("interrupt", "condition")))
      download(...)
    })
    first <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_identical(first$state, "interrupted")
    expect_false(first$complete)
    expect_equal(first$resources$status, c("downloaded", "interrupted"))
    second <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(second$complete)
    expect_equal(second$resources$status, c("reused", "downloaded"))
    expect_length(downloads(), 2L)
  })
})

test_that("source changes during transfer retain only a partial archive", {
  executor_fixture(function(plan, client, control, downloads) {
    writeLines('{"mode":"change_after"}', control)
    result <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_false(result$complete)
    expect_equal(result$resources$status, c("source_changed", "pending"))
    expect_true(file.exists(result$resources$partial_path[1]))
    expect_false(file.exists(plan$resources$destination[1]))
  })
})

test_that("source changes stop downloads even when continuing ordinary failures", {
  for (backend in c("httr2", "curl")) executor_fixture(function(plan, client, control, downloads) {
    writeLines('{"mode":"change_after"}', control)
    result <- xnat_execute_plan(plan, client, progress = FALSE, continue_on_error = TRUE)
    expect_false(result$complete)
    expect_false(result$source_verified)
    expect_equal(result$resources$status, c("source_changed", "pending"))
    expect_equal(result$resources$error_class[1], "xnatR_source_changed")
    expect_true(file.exists(result$resources$partial_path[1]))
    expect_false(any(file.exists(plan$resources$destination)))
    expect_length(downloads(), 1L)
  }, backend)
})

test_that("ordinary transfer failures can continue to later resources", {
  executor_fixture(function(plan, client, control, downloads) {
    writeLines('{"mode":"truncate","target":"E1"}', control)
    result <- xnat_execute_plan(plan, client, progress = FALSE, continue_on_error = TRUE)
    expect_false(result$complete)
    expect_equal(result$resources$status, c("failed", "downloaded"))
    expect_equal(result$verified_resources, 1L)
    expect_length(downloads(), 2L)
  })
})

test_that("download timeouts apply to both transport backends", {
  for (backend in c("httr2", "curl")) executor_fixture(function(plan, client, control, downloads) {
    writeLines('{"mode":"slow"}', control)
    result <- xnat_execute_plan(plan, client, download_timeout = 0.1, progress = FALSE)
    expect_false(result$complete)
    expect_equal(result$resources$error_class[1], "xnatR_network_error")
    expect_false(file.exists(plan$resources$destination[1]))
  }, backend)
})

test_that("cross-process locks prevent writes and release on process termination", {
  executor_fixture(function(plan, client, control, downloads) {
    dir.create(plan$destination_root)
    ready <- tempfile()
    lock_path <- file.path(plan$destination_root, ".xnatr-executor.lock")
    script <- "library(filelock); a<-commandArgs(TRUE); l<-lock(a[1]); writeLines('ready',a[2]); Sys.sleep(60)"
    proc <- processx::process$new(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", "-e", script, lock_path, ready), stdout = "|", stderr = "|")
    on.exit({proc$kill(); unlink(ready)}, add = TRUE)
    wait_for_file(ready, proc)
    expect_true(file.exists(ready))
    expect_error(xnat_execute_plan(plan, client, progress = FALSE), class = "xnatR_lock_error")
    expect_length(downloads(), 0L)
    proc$kill()
    proc$wait(timeout = 5000)
    result <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(result$complete)
  })
})

test_that("executor refuses symlinked destinations", {
  skip_on_os("windows")
  executor_fixture(function(plan, client, control, downloads) {
    dir.create(plan$destination_root)
    elsewhere <- tempfile()
    dir.create(elsewhere)
    on.exit(unlink(elsewhere, recursive = TRUE), add = TRUE)
    link <- file.path(plan$destination_root, plan_id("P"))
    expect_true(file.symlink(elsewhere, link))
    result <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_false(result$complete)
    expect_length(downloads(), 0L)
    expect_length(list.files(elsewhere, all.files = TRUE, no.. = TRUE), 0L)
  })
})

test_that("saved plans and in-memory plans share receipts and full completeness", {
  executor_fixture(function(plan, client, control, downloads) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    xnat_write_plan(plan, path)
    first <- xnat_execute_plan(path, client, progress = FALSE)
    expect_true(first$complete)
    second <- xnat_execute_plan(plan, client, progress = FALSE)
    expect_true(second$complete)
    expect_identical(first$plan_sha256, second$plan_sha256)
    expect_equal(second$resources$status, c("reused", "reused"))
    expect_length(downloads(), 2L)
  })
})

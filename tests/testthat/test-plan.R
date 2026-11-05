test_that("exact DICOM plans include catalogs, totals and explicit scan exclusions", {
  local_mocked_bindings(xnat_get = fixture_get(inventory_fixture()))
  inv <- fixture_inventory()
  plan <- fixture_plan(inv)
  expect_s3_class(plan, "xnat_download_plan")
  expect_equal(plan$sessions$experiment_id, c("E1", "E2"))
  expect_equal(plan$expected_resources, 2L)
  expect_equal(plan$expected_files, 4L)
  expect_equal(plan$expected_bytes, "60")
  expect_equal(plan$scans$disposition, rep(c("included", "no_dicom"), 2))
  expect_equal(plan$resources$label, c("DICOM", "DICOM"))
  expect_equal(plan$resources$expected_bytes, c("30", "30"))
  expect_match(plan$resources$path[1], "data/experiments/E1/scans/1/resources/100/files", fixed = TRUE)
  expect_false(dir.exists(plan$destination_root))
  expect_no_error(validate_download_plan(plan))
  expect_no_error(capture.output(print(plan)))
  expect_error(fixture_plan(inv, ids = "visit"), "canonical experiment")
  expect_error(fixture_plan(inv, ids = "E3"), "unresolved")
  expect_error(fixture_plan(inv, ids = c("E1", "E1")), "unique")
  subset <- fixture_plan(inv, ids = "E1", scans = data.frame(experiment_id = "E1", scan_id = "1"))
  expect_equal(nrow(subset$scans), 1L)
  expect_error(fixture_plan(inv, scans = data.frame(experiment_id = "E1", scan_id = "1")), "Every selected session")
})

test_that("saved plans round-trip offline and detect edits and path substitution", {
  local_mocked_bindings(xnat_get = fixture_get(inventory_fixture()))
  plan <- fixture_plan(fixture_inventory())
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  xnat_write_plan(plan, path)
  expect_error(xnat_write_plan(plan, path), "already exists")
  local_mocked_bindings(xnat_get = function(...) stop("must be offline"))
  restored <- xnat_read_plan(path)
  expect_equal(restored, plan, ignore_attr = TRUE)
  raw <- readLines(path)
  expect_false(any(grepl("password|credential|JSESSION", raw)))
  expect_true(any(grepl('"resource_id": "100"', raw, fixed = TRUE)))
  envelope <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  envelope$plan$resources[[1]]$destination <- "/tmp/elsewhere.zip"
  writeLines(plan_json(envelope), path)
  expect_error(xnat_read_plan(path), "fingerprint")
  envelope$sha256 <- plan_hash(envelope$plan)
  writeLines(plan_json(envelope), path)
  expect_error(xnat_read_plan(path), "paths disagree")
})

test_that("changed sources and catalog mismatches are refused", {
  tables <- inventory_fixture()
  local_mocked_bindings(xnat_get = fixture_get(tables))
  inv <- fixture_inventory()
  changed <- tables
  changed[["data/projects/P/experiments"]]$date[1] <- "2026-02-27"
  local_mocked_bindings(xnat_get = fixture_get(changed))
  expect_error(fixture_plan(inv), "metadata changed")
  changed <- tables
  changed[["data/projects/P/subjects/S1/experiments"]] <- tibble::tibble(ID = "E2")
  local_mocked_bindings(xnat_get = fixture_get(changed))
  expect_error(fixture_plan(inv), "membership")
  changed <- tables
  changed[["data/experiments/E1/scans/1/resources/100/files"]]$Size[1] <- "11"
  local_mocked_bindings(xnat_get = fixture_get(changed))
  expect_error(fixture_plan(inv), "disagrees")
  changed[["data/experiments/E1/scans/1/resources/100/files"]]$Size[1] <- NA_character_
  local_mocked_bindings(xnat_get = fixture_get(changed))
  expect_error(fixture_plan(inv), "unknown")
  changed <- tables
  changed[["data/experiments/E1/scans/1/resources"]]$xnat_abstractresource_id[1] <- "changed"
  local_mocked_bindings(xnat_get = fixture_get(changed))
  expect_error(fixture_plan(inv), "resource identities changed")
})

test_that("decimal sizes retain exact large integers and destinations cannot traverse", {
  expect_equal(decimal_sum(c("99999999999999999999", "2")), "100000000000000000001")
  expect_equal(decimal_sum(character()), "0")
  expect_equal(decimal_sum(c("0001", "9")), "10")
  expect_error(decimal_values(c("1.5")), "decimal")
  keys <- plan_resource_key(c("../P", "P", "p"), rep("E", 3), rep("1", 3), rep("100", 3))
  expect_equal(length(unique(tolower(keys))), 3L)
  expect_false(any(grepl("..", keys, fixed = TRUE)))
  expect_error(plan_root("../escape"), "parent traversal")
})

test_that("plan export refuses unexpected fields instead of serializing credentials", {
  local_mocked_bindings(xnat_get = fixture_get(inventory_fixture()))
  plan <- fixture_plan(fixture_inventory())
  plan$client <- xnat_client("https://example.org", "u", "secret")
  expect_error(xnat_write_plan(plan, tempfile()), "Unexpected")
})

test_that("large file sizes survive metadata JSON decoding, planning and export", {
  tables <- inventory_fixture()
  tables[["data/experiments/E1/scans/1/resources/100/files"]]$Size <- c("9007199254740993", "20")
  tables[["data/experiments/E1/scans/1/resources"]]$file_size[1] <- "9007199254741013"
  local_mocked_bindings(xnat_get = fixture_get(tables))
  plan <- fixture_plan(fixture_inventory(), ids = "E1")
  expect_equal(plan$expected_bytes, "9007199254741013")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  xnat_write_plan(plan, path)
  expect_equal(xnat_read_plan(path)$files$bytes, c("9007199254740993", "20"))
})

test_that("both transports discover and save plans on paged and legacy loopback servers", {
  skip_if(Sys.which("python3") == "", "python3 unavailable")
  scratch <- tempfile()
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  fixture <- file.path(scratch, "fixture.json")
  writeLines(plan_json(inventory_fixture()), fixture)
  for (mode in c("paged", "legacy")) {
    ready <- file.path(scratch, paste0(mode, "-port"))
    server <- start_fixture_server("inventory-server.py", c(fixture, mode), ready)
    tryCatch({
      url <- server$url
      for (backend in c("httr2", "curl")) {
        client <- xnat_client(url, "synthetic", "synthetic", backend = backend)
        inv <- xnat_inventory(date_from = "2026-02-01", date_to = "2026-03-31",
          include_unknown_dates = FALSE, client = client, page_size = 2)
        expect_equal(inv$sessions$experiment_id, c("E1", "E2", "E5"))
        plan <- xnat_plan_download(inv, c("E1", "E2"), file.path(scratch, "archives"), client = client)
        expect_equal(plan$expected_files, 4L)
        path <- file.path(scratch, paste0(mode, "-", backend, ".json"))
        xnat_write_plan(plan, path)
        expect_equal(xnat_read_plan(path)$resources, plan$resources)
        expect_false(dir.exists(plan$destination_root))
      }
    }, finally = server$proc$kill())
  }
})

test_that("scan metadata changes require selection review", {
  tables <- inventory_fixture()
  local_mocked_bindings(xnat_get = fixture_get(tables))
  inv <- fixture_inventory()
  tables[["data/experiments/E1/scans"]]$series_description[1] <- "different task"
  local_mocked_bindings(xnat_get = fixture_get(tables))
  expect_error(fixture_plan(inv), "scan metadata changed")
})

test_that("ordinary numeric sizes do not become scientific notation", {
  tables <- inventory_fixture()
  tables[["data/experiments/E1/scans/1/resources/100/files"]]$Size <- c(10000000, 20000000)
  tables[["data/experiments/E1/scans/1/resources"]]$file_size <- c(30000000, 5)
  local_mocked_bindings(xnat_get = fixture_get(tables))
  plan <- fixture_plan(fixture_inventory(), ids = "E1")
  expect_equal(plan$files$bytes, c("10000000", "20000000"))
  expect_equal(plan$expected_bytes, "30000000")
})

# Tests for the fzf explorer. fzf and a live server are never invoked: the
# interactive seam (.fzf_run) is replaced by a scripted fake `run_fn`, and the
# list_*() / dicom_dump() / download_experiment() data sources are mocked. Only
# the system2() call to fzf itself is left unverified here (it needs a real
# terminal + fzf binary).

# A fake fzf runner that returns scripted results in order.
fake_runner <- function(...) {
  responses <- list(...)
  i <- 0L
  function(args, input) {
    i <<- i + 1L
    if (i > length(responses)) {
      stop("fake_runner called more times than scripted")
    }
    responses[[i]]
  }
}

# Build a plausible fzf stdout vector: query, action key, then selected lines.
fzf_out <- function(rows, action = "", query = "", status = 0L) {
  selected <- paste0(rows, "\tdisplay")
  list(stdout = c(query, action, selected), status = status)
}

abort_out <- function() list(stdout = c("", ""), status = 130L)

# ---- pure rendering / parsing helpers -------------------------------------

test_that(".fzf_render_lines prefixes a hidden index and shows chosen columns", {
  lines <- xnatR:::.fzf_render_lines(mock_projects(3), c("ID", "name"))
  expect_length(lines, 3)
  expect_true(startsWith(lines[[1]], "1\t"))
  expect_match(lines[[2]], "PROJECT_2")
  expect_match(lines[[2]], "Test Project 2")
})

test_that(".fzf_build_args encodes multi, expect keys, preview, hidden index", {
  args <- xnatR:::.fzf_build_args(
    header = "crumb", multi = TRUE,
    expect = c("ctrl-s", "ctrl-d", "ctrl-e", "ctrl-o"), preview_dir = "/tmp/p"
  )
  expect_true("--multi" %in% args)
  expect_true("--with-nth" %in% args)
  expect_true("--expect" %in% args)
  expect_true(any(grepl("ctrl-d", args)))
  expect_true(any(grepl("^cat ", args)))
  expect_true(any(grepl("\\{1\\}", args)))
})

test_that(".preview_detail and .fzf_write_previews round-trip per-row detail", {
  contents <- xnatR:::.preview_detail(mock_projects(3))
  expect_length(contents, 3)
  expect_true(any(grepl("^ID: PROJECT_2$", contents[[2]])))

  dir <- withr::local_tempdir()
  xnatR:::.fzf_write_previews(contents, dir)
  expect_setequal(list.files(dir), c("1", "2", "3"))
  expect_true(any(grepl("^name: Test Project 2$", readLines(file.path(dir, "2")))))
})

test_that(".fzf_parse_select maps lines to row indices and reads the action", {
  parsed <- xnatR:::.fzf_parse_select(fzf_out(rows = c(2, 5), action = "ctrl-d", query = "q"))
  expect_equal(parsed$rows, c(2L, 5L))
  expect_equal(parsed$action, "ctrl-d")
  expect_false(parsed$aborted)
  expect_equal(parsed$query, "q")

  enter <- xnatR:::.fzf_parse_select(fzf_out(rows = 3))
  expect_equal(enter$action, "")
  expect_equal(enter$rows, 3L)

  expect_true(xnatR:::.fzf_parse_select(abort_out())$aborted)
  expect_true(xnatR:::.fzf_parse_select(list(stdout = c("q", ""), status = 0L))$aborted)
})

# ---- context / navigation helpers -----------------------------------------

test_that(".resolve_subject_id finds the subject column or returns NA", {
  row <- mock_experiments(1)[1, , drop = FALSE]
  expect_equal(xnatR:::.resolve_subject_id(row), "SUBJECT_1")
  expect_true(is.na(xnatR:::.resolve_subject_id(mock_projects(1)[1, , drop = FALSE])))
})

test_that(".explore_initial_stack seeds the right level and validates ids", {
  expect_equal(xnatR:::.explore_initial_stack()[[1]]$level, "projects")
  expect_equal(xnatR:::.explore_initial_stack(project_id = "P")[[1]]$level, "subjects")
  expect_equal(xnatR:::.explore_initial_stack("P", "S")[[1]]$level, "experiments")

  scans <- xnatR:::.explore_initial_stack("P", "S", "E")[[1]]
  expect_equal(scans$level, "scans")
  expect_equal(scans$ctx$experiment_id, "E")

  expect_error(xnatR:::.explore_initial_stack(subject_id = "S"), "project_id")
  expect_error(xnatR:::.explore_initial_stack(project_id = "P", experiment_id = "E"), "subject_id")
})

test_that(".explore_web_url builds REST paths per level", {
  expect_equal(
    xnatR:::.explore_web_url("https://x.org", "experiments", "E1", list()),
    "https://x.org/data/experiments/E1"
  )
  expect_equal(
    xnatR:::.explore_web_url("https://x.org", "scans", "1", list(experiment_id = "E1")),
    "https://x.org/data/experiments/E1/scans/1"
  )
  expect_true(is.na(xnatR:::.explore_web_url(NULL, "projects", "P", list())))
})

test_that(".preview_scan_dicom renders DICOM headers per scan", {
  local_mocked_bindings(
    dicom_dump = function(experiment_id, scan_id = NULL, fields = NULL, client = NULL) {
      expect_equal(experiment_id, "E1")
      mock_dicom_dump()
    },
    .package = "xnatR"
  )
  blocks <- xnatR:::.preview_scan_dicom(mock_scans(2), list(experiment_id = "E1"), client = NULL)
  expect_length(blocks, 2)
  expect_true(any(grepl("^Modality: MR$", blocks[[1]])))
  expect_true(any(grepl("Repetition Time: 2000", blocks[[1]])))
})

# ---- guards ----------------------------------------------------------------

test_that("xnat_explore errors when not interactive", {
  expect_error(xnat_explore(.interactive = FALSE), "interactive")
})

test_that("xnat_explore errors when fzf is unavailable", {
  local_mocked_bindings(.fzf_available = function() FALSE, .package = "xnatR")
  expect_error(xnat_explore(.interactive = TRUE), "fzf")
})

# ---- navigation state machine ---------------------------------------------

test_that("xnat_explore descends, then returns the Ctrl-S selection", {
  local_mocked_bindings(
    .fzf_available = function() TRUE,
    list_projects = function(...) mock_projects(3),
    list_subjects = function(project_id, ...) {
      expect_equal(project_id, "PROJECT_2")
      mock_subjects(3, project_id = project_id)
    },
    .package = "xnatR"
  )

  runner <- fake_runner(
    fzf_out(rows = 2, action = ""),               # Enter: open PROJECT_2
    fzf_out(rows = c(1, 3), action = "ctrl-s")    # Ctrl-S: return subjects 1 and 3
  )
  result <- xnat_explore(.interactive = TRUE, run_fn = runner)

  expect_equal(nrow(result), 2)
  expect_equal(result$ID, c("SUBJECT_1", "SUBJECT_3"))
})

test_that("xnat_explore Ctrl-D downloads marked sessions and keeps browsing", {
  downloaded <- character()
  local_mocked_bindings(
    .fzf_available = function() TRUE,
    list_experiments = function(project_id, subject_id = NULL, ...) mock_experiments(2),
    download_experiment = function(experiment_id, ...) {
      downloaded <<- c(downloaded, experiment_id)
      file.path(tempdir(), paste0(experiment_id, ".zip"))
    },
    .package = "xnatR"
  )

  runner <- fake_runner(
    fzf_out(rows = c(1, 2), action = "ctrl-d"),   # download both sessions
    abort_out()                                   # Esc at the (seeded) root -> exit
  )
  result <- xnat_explore(
    project_id = "P1", subject_id = "S1",
    .interactive = TRUE, run_fn = runner
  )

  expect_null(result)
  expect_equal(downloaded, c("EXP_1", "EXP_2"))
})

test_that("xnat_explore Ctrl-E jumps to all sessions in the project", {
  seen_subject <- "unset"
  local_mocked_bindings(
    .fzf_available = function() TRUE,
    list_subjects = function(project_id, ...) mock_subjects(2, project_id = project_id),
    list_experiments = function(project_id, subject_id = NULL, ...) {
      seen_subject <<- subject_id
      mock_experiments(2, project_id = project_id)
    },
    .package = "xnatR"
  )

  runner <- fake_runner(
    fzf_out(rows = 1, action = "ctrl-e"),         # jump to all sessions
    fzf_out(rows = 1, action = "ctrl-s")          # return the first session
  )
  result <- xnat_explore(project_id = "P1", .interactive = TRUE, run_fn = runner)

  expect_null(seen_subject)                        # listed with no subject filter
  expect_equal(result$ID, "EXP_1")
})

test_that("xnat_explore Enter on a leaf scan returns that scan", {
  local_mocked_bindings(
    .fzf_available = function() TRUE,
    list_scans = function(project_id, subject_id, experiment_id, ...) mock_scans(3),
    dicom_dump = function(experiment_id, scan_id = NULL, fields = NULL, client = NULL) mock_dicom_dump(),
    .package = "xnatR"
  )

  runner <- fake_runner(fzf_out(rows = 2, action = ""))
  result <- xnat_explore(
    project_id = "P1", subject_id = "S1", experiment_id = "E1",
    .interactive = TRUE, run_fn = runner
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, "2")
})

test_that("xnat_explore returns NULL when aborted at the root", {
  local_mocked_bindings(
    .fzf_available = function() TRUE,
    list_projects = function(...) mock_projects(3),
    .package = "xnatR"
  )
  runner <- fake_runner(abort_out())
  expect_null(xnat_explore(.interactive = TRUE, run_fn = runner))
})

# ---- real fzf (runs only where the binary is installed) -------------------

test_that("real fzf accepts the built command line (filter mode)", {
  skip_if_not(nzchar(Sys.which("fzf")), "fzf not installed")

  dir <- withr::local_tempdir()
  xnatR:::.fzf_write_previews(list("a detail", "b detail", "c detail"), dir)
  args <- c(
    xnatR:::.fzf_build_args(
      header = "h", multi = TRUE,
      expect = c("ctrl-s", "ctrl-d", "ctrl-e", "ctrl-o"), preview_dir = dir
    ),
    "--filter=Beta"
  )

  res <- xnatR:::.fzf_run(args, c("1\tProject Alpha", "2\tProject Beta", "3\tGamma"))

  expect_equal(res$status, 0L)
  expect_match(paste(res$stdout, collapse = "\n"), "Project Beta")
})

test_that("xnat_explore goes back up a level on abort, then exits", {
  local_mocked_bindings(
    .fzf_available = function() TRUE,
    list_projects = function(...) mock_projects(2),
    list_subjects = function(project_id, ...) mock_subjects(2, project_id = project_id),
    .package = "xnatR"
  )
  runner <- fake_runner(
    fzf_out(rows = 1, action = ""),               # open PROJECT_1 -> subjects
    abort_out(),                                  # Esc -> back to projects
    abort_out()                                   # Esc -> exit
  )
  expect_null(xnat_explore(.interactive = TRUE, run_fn = runner))
})

# fzf-powered interactive explorer for XNAT.
#
# fzf does the hard interactive work (fuzzy search, multi-select, preview pane);
# xnatR supplies the data (via the list_*() functions) and the actions.
#
# Architecture: a modal, R-orchestrated loop. R holds the navigation state and
# invokes fzf once per level as a picker (data is fetched once per level, in
# process -- no fresh R per keystroke). Every fzf interaction is funnelled
# through .fzf_run(), the ONLY function that shells out to fzf, so the rest --
# rendering, argument building, result parsing, previews, and the navigation
# state machine -- stays pure and is testable offline with a scripted fake
# `run_fn`, without fzf installed or a live server.

# DICOM tags shown in the scan-level preview (hex GGGGEEEE).
.explore_scan_preview_fields <- c(
  "00080060", # Modality
  "0008103E", # Series Description
  "00180024", # Sequence Name
  "00181030", # Protocol Name
  "00180080", # Repetition Time
  "00180081", # Echo Time
  "00181314", # Flip Angle
  "00180050", # Slice Thickness
  "00280010", # Rows
  "00280011"  # Columns
)

#' Is the fzf binary available on PATH?
#' @noRd
.fzf_available <- function() {
  nzchar(Sys.which("fzf"))
}

#' Render a data frame to tab-delimited fzf input lines
#'
#' Field 1 is the 1-based row index. fzf is told to display and search only from
#' field 2 on (`--with-nth=2..`), so the index is hidden from the user yet
#' returned with the selection, letting us map a chosen line back to its row.
#' @noRd
.fzf_render_lines <- function(df, display_cols) {
  cols <- intersect(display_cols, names(df))
  if (length(cols) == 0) {
    cols <- names(df)
  }
  values <- lapply(df[cols], function(col) as.character(col))
  display <- do.call(paste, c(values, sep = "  "))
  paste(seq_len(nrow(df)), display, sep = "\t")
}

#' Build per-row preview content from a data frame's own columns
#' @return A list (one element per row) of character vectors.
#' @noRd
.preview_detail <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    vapply(
      names(row),
      function(nm) paste0(nm, ": ", as.character(row[[nm]])),
      character(1)
    )
  })
}

#' Build per-scan preview content from DICOM headers
#'
#' Calls [dicom_dump()] once per scan (bounded by the scan count) on level entry,
#' falling back to the row's own columns if a dump fails or is empty.
#' @return A list (one element per row) of character vectors.
#' @noRd
.preview_scan_dicom <- function(df, ctx, client, fields = .explore_scan_preview_fields) {
  id_col <- if ("ID" %in% names(df)) "ID" else names(df)[[1]]
  lapply(seq_len(nrow(df)), function(i) {
    scan_id <- as.character(df[[id_col]][[i]])
    dump <- tryCatch(
      dicom_dump(ctx$experiment_id, scan_id = scan_id, fields = fields, client = client),
      error = function(e) NULL
    )
    if (is.null(dump) || nrow(dump) == 0) {
      return(.preview_detail(df[i, , drop = FALSE])[[1]])
    }
    label_col <- intersect(c("desc", "tag1"), names(dump))
    value_col <- intersect("value", names(dump))
    if (length(label_col) == 0 || length(value_col) == 0) {
      return(.preview_detail(df[i, , drop = FALSE])[[1]])
    }
    c(
      paste0("Scan ", scan_id),
      "",
      paste0(as.character(dump[[label_col[[1]]]]), ": ", as.character(dump[[value_col[[1]]]]))
    )
  })
}

#' Write per-row preview files named by row index
#' @param contents A list of character vectors (one per row).
#' @noRd
.fzf_write_previews <- function(contents, dir) {
  for (i in seq_along(contents)) {
    writeLines(contents[[i]], file.path(dir, as.character(i)))
  }
  invisible(dir)
}

#' Build the fzf argument vector
#' @noRd
.fzf_build_args <- function(header = NULL, multi = TRUE, expect = NULL, preview_dir = NULL) {
  args <- c("--delimiter", "\t", "--with-nth", "2..", "--print-query", "--reverse")
  if (isTRUE(multi)) {
    args <- c(args, "--multi")
  }
  if (!is.null(header) && nzchar(header)) {
    args <- c(args, "--header", header)
  }
  if (!is.null(expect) && length(expect) > 0) {
    args <- c(args, "--expect", paste(expect, collapse = ","))
  }
  if (!is.null(preview_dir)) {
    # {1} is field 1 (the row index) of the highlighted line.
    args <- c(args, "--preview", paste0("cat ", shQuote(file.path(preview_dir, "{1}"))))
  }
  args
}

#' The seam: run fzf with the given args, feeding `input` on stdin
#'
#' fzf reads candidates from stdin and drives its UI on /dev/tty, so capturing
#' stdout here returns only the selection, not the interface.
#' @return A list with `stdout` (character vector) and `status` (integer).
#' @noRd
.fzf_run <- function(args, input) {
  # system2() pastes args into a shell command line without quoting them, so any
  # arg containing a space (notably the --preview command) must be quoted here or
  # the shell splits it. Quoting space-free args is harmless.
  out <- suppressWarnings(
    system2("fzf", args = shQuote(args), input = input, stdout = TRUE, stderr = FALSE)
  )
  list(stdout = as.character(out), status = attr(out, "status") %||% 0L)
}

#' Parse the result of an fzf run into a normalised selection
#'
#' With `--print-query` and `--expect`, fzf prints: the query, then the pressed
#' action key (empty for a plain Enter), then the selected lines.
#' @return A list: `query`, `action` ("" for Enter), `rows` (integer indices),
#'   `aborted` (TRUE if cancelled / no match).
#' @noRd
.fzf_parse_select <- function(res, expect = NULL) {
  out <- res$stdout
  aborted <- res$status %in% c(1L, 130L) # 130 = Esc/Ctrl-C, 1 = no match

  query <- if (length(out) >= 1) out[[1]] else ""
  action <- if (length(out) >= 2) out[[2]] else ""
  selected <- if (length(out) >= 3) out[-(1:2)] else character()

  rows <- suppressWarnings(as.integer(sub("\t.*$", "", selected)))
  rows <- rows[!is.na(rows)]
  if (length(rows) == 0) {
    aborted <- TRUE
  }

  list(query = query, action = action, rows = rows, aborted = aborted)
}

#' Resolve a subject identifier from an experiment listing row
#' @noRd
.resolve_subject_id <- function(row) {
  candidates <- c("subject_ID", "subject_id", "subject_label", "subject")
  for (nm in candidates) {
    if (nm %in% names(row)) {
      value <- as.character(row[[nm]][[1]])
      if (!is.na(value) && nzchar(value)) {
        return(value)
      }
    }
  }
  NA_character_
}

# Hierarchy levels. Each level knows how to fetch its listing from the
# accumulated context, which columns to display, the identifying column, the
# context key set when descending, its child level (NULL = leaf), and an
# optional on_open() to enrich context from the chosen row.
.explore_levels <- function() {
  list(
    projects = list(
      fetch = function(ctx, client) list_projects(client = client),
      display = c("ID", "name", "description"),
      id = "ID", ctx_key = "project_id", child = "subjects"
    ),
    subjects = list(
      fetch = function(ctx, client) list_subjects(ctx$project_id, client = client),
      display = c("ID", "label", "gender", "age"),
      id = "ID", ctx_key = "subject_id", child = "experiments"
    ),
    experiments = list(
      fetch = function(ctx, client) list_experiments(ctx$project_id, ctx$subject_id, client = client),
      display = c("ID", "label", "date", "xsiType"),
      id = "ID", ctx_key = "experiment_id", child = "scans",
      on_open = function(row, ctx) {
        if (is.null(ctx$subject_id)) {
          sid <- .resolve_subject_id(row)
          if (!is.na(sid)) ctx$subject_id <- sid
        }
        ctx
      }
    ),
    scans = list(
      fetch = function(ctx, client) {
        list_scans(ctx$project_id, ctx$subject_id, ctx$experiment_id, client = client)
      },
      display = c("ID", "type", "series_description", "quality"),
      id = "ID", ctx_key = "scan_id", child = NULL
    )
  )
}

.explore_breadcrumb <- function(ctx, level) {
  trail <- c(ctx$project_id, ctx$subject_id, ctx$experiment_id)
  crumb <- if (length(trail) > 0) paste(trail, collapse = " / ") else "(root)"
  keys <- "Enter open | Tab mark | C-s return | C-d download | C-e all sessions | C-o web | Esc back"
  paste0(crumb, "  [", level, "]\n", keys)
}

#' Seed the initial navigation stack from optional starting ids
#' @noRd
.explore_initial_stack <- function(project_id = NULL, subject_id = NULL, experiment_id = NULL) {
  if (!is.null(experiment_id) && (is.null(project_id) || is.null(subject_id))) {
    cli::cli_abort("{.arg experiment_id} requires both {.arg project_id} and {.arg subject_id}.")
  }
  if (!is.null(subject_id) && is.null(project_id)) {
    cli::cli_abort("{.arg subject_id} requires {.arg project_id}.")
  }

  ctx <- list()
  level <- "projects"
  if (!is.null(project_id)) {
    ctx$project_id <- project_id
    level <- "subjects"
  }
  if (!is.null(subject_id)) {
    ctx$subject_id <- subject_id
    level <- "experiments"
  }
  if (!is.null(experiment_id)) {
    ctx$experiment_id <- experiment_id
    level <- "scans"
  }
  list(list(level = level, ctx = ctx))
}

#' Build a best-effort XNAT web URL for a row (REST path; deep app links are
#' server-version specific and a future refinement)
#' @noRd
.explore_web_url <- function(base_url, level, id, ctx) {
  if (is.null(base_url) || !nzchar(base_url)) {
    return(NA_character_)
  }
  switch(level,
    projects = file.path(base_url, "data", "projects", id),
    subjects = file.path(base_url, "data", "projects", ctx$project_id, "subjects", id),
    experiments = file.path(base_url, "data", "experiments", id),
    scans = file.path(base_url, "data", "experiments", ctx$experiment_id, "scans", id),
    base_url
  )
}

#' Download the selected rows (Ctrl-D action); stays in the explorer loop
#' @noRd
.explore_download <- function(level, rows_df, ctx, dest_dir, client) {
  if (level == "experiments") {
    ids <- as.character(rows_df[["ID"]])
    for (id in ids) {
      download_experiment(experiment_id = id, dest_dir = dest_dir, client = client)
    }
    cli::cli_alert_success("Downloaded {length(ids)} session{?s} to {.file {dest_dir}}.")
  } else if (level == "scans") {
    scan_ids <- paste(as.character(rows_df[["ID"]]), collapse = ",")
    download_experiment(
      experiment_id = ctx$experiment_id, scan_id = scan_ids,
      dest_dir = dest_dir, client = client
    )
    cli::cli_alert_success("Downloaded scan{?s} {.val {scan_ids}} to {.file {dest_dir}}.")
  } else {
    cli::cli_alert_info("Nothing downloadable at the {.field {level}} level; open a session or scan.")
  }
  invisible(NULL)
}

#' Run an fzf selection over a data frame
#' @noRd
.fzf_select <- function(df, display_cols, preview_content = NULL, header = NULL,
                        multi = TRUE, expect = NULL, run_fn = .fzf_run) {
  lines <- .fzf_render_lines(df, display_cols)

  if (is.null(preview_content)) {
    preview_content <- .preview_detail(df)
  }
  preview_dir <- tempfile("xnat_fzf_")
  dir.create(preview_dir)
  on.exit(unlink(preview_dir, recursive = TRUE), add = TRUE)
  .fzf_write_previews(preview_content, preview_dir)

  args <- .fzf_build_args(header = header, multi = multi, expect = expect, preview_dir = preview_dir)
  res <- run_fn(args, lines)
  .fzf_parse_select(res, expect = expect)
}

#' Explore XNAT interactively with fzf
#'
#' Drills down the project -> subject -> experiment -> scan hierarchy using fzf
#' for fuzzy search and multi-select, with a preview pane showing the highlighted
#' row's detail (DICOM headers at the scan level).
#'
#' Keys: `Enter` opens the highlighted item; `Tab` marks rows; `Ctrl-S` returns
#' the marked rows (e.g. to feed [download_experiment()]); `Ctrl-D` downloads the
#' marked rows immediately and stays in the explorer; `Ctrl-E` (at the subject
#' level) jumps to all sessions in the project; `Ctrl-O` opens the highlighted
#' item in the XNAT web UI; `Esc` goes back a level (or exits at the top).
#'
#' Requires the `fzf` binary on your `PATH` (e.g. `brew install fzf`) and an
#' interactive session.
#'
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @param project_id,subject_id,experiment_id Optional starting point; supplying
#'   them skips the corresponding levels.
#' @param dest_dir Destination directory for the `Ctrl-D` download action.
#' @param .interactive If `FALSE`, the function errors rather than prompting.
#' @param run_fn Internal seam for invoking fzf; overridden in tests.
#'
#' @return A tibble of the rows selected with `Ctrl-S` (or `Enter` on a leaf
#'   scan), or `NULL` if you exit without selecting.
#'
#' @examples
#' \dontrun{
#' xnat_connect(base_url = "https://central.xnat.org",
#'              username = "guest", password = "guest")
#'
#' chosen <- xnat_explore()
#' # Jump straight to a known session's scans:
#' xnat_explore(project_id = "CENTRAL_OASIS", subject_id = "OAS1_0001",
#'              experiment_id = "OAS1_0001_MR1")
#' }
#'
#' @export
xnat_explore <- function(client = NULL,
                         project_id = NULL, subject_id = NULL, experiment_id = NULL,
                         dest_dir = ".",
                         .interactive = interactive(), run_fn = .fzf_run) {
  if (!isTRUE(.interactive)) {
    cli::cli_abort(c(
      "{.fn xnat_explore} is interactive.",
      "i" = "Use the {.fn list_projects} / {.fn list_subjects} family for programmatic access."
    ))
  }
  if (!.fzf_available()) {
    cli::cli_abort(c(
      "Could not find {.code fzf} on your PATH.",
      "i" = "Install it (e.g. {.code brew install fzf} or {.code apt install fzf}) and try again."
    ))
  }

  levels <- .explore_levels()
  stack <- .explore_initial_stack(project_id, subject_id, experiment_id)

  repeat {
    frame <- stack[[length(stack)]]
    cfg <- levels[[frame$level]]
    ctx <- frame$ctx

    df <- cfg$fetch(ctx, client)

    if (is.null(df) || nrow(df) == 0) {
      cli::cli_alert_warning("No {frame$level} found.")
      if (length(stack) == 1) {
        return(invisible(NULL))
      }
      stack[[length(stack)]] <- NULL
      next
    }

    preview_content <- if (frame$level == "scans") {
      .preview_scan_dicom(df, ctx, client)
    } else {
      .preview_detail(df)
    }

    sel <- .fzf_select(
      df,
      display_cols = cfg$display,
      preview_content = preview_content,
      header = .explore_breadcrumb(ctx, frame$level),
      multi = TRUE,
      expect = c("ctrl-s", "ctrl-d", "ctrl-e", "ctrl-o"),
      run_fn = run_fn
    )

    # Esc / no selection -> back up a level (exit at the root).
    if (sel$aborted) {
      if (length(stack) == 1) {
        return(invisible(NULL))
      }
      stack[[length(stack)]] <- NULL
      next
    }

    chosen <- df[sel$rows, , drop = FALSE]

    # Ctrl-S: return the marked rows to the caller.
    if (identical(sel$action, "ctrl-s")) {
      return(chosen)
    }

    # Ctrl-D: download the marked rows now, then keep browsing.
    if (identical(sel$action, "ctrl-d")) {
      .explore_download(frame$level, chosen, ctx, dest_dir, client)
      next
    }

    # Ctrl-E: jump to every session in the project (no subject filter).
    if (identical(sel$action, "ctrl-e")) {
      if (is.null(ctx$project_id)) {
        cli::cli_alert_info("Open a project first.")
        next
      }
      jump_ctx <- list(project_id = ctx$project_id)
      stack[[length(stack) + 1L]] <- list(level = "experiments", ctx = jump_ctx)
      next
    }

    # Ctrl-O: open the highlighted row in the web UI; keep browsing.
    if (identical(sel$action, "ctrl-o")) {
      base_url <- if (is.null(client)) xnatR_env$base_url else client$base_url
      url <- .explore_web_url(base_url, frame$level, chosen[[cfg$id]][[1]], ctx)
      if (is.na(url)) {
        cli::cli_alert_warning("No server URL available.")
      } else {
        utils::browseURL(url)
      }
      next
    }

    # Enter: open the highlighted row.
    opened <- chosen[1, , drop = FALSE]
    if (is.null(cfg$child)) {
      return(opened)
    }
    child_ctx <- ctx
    child_ctx[[cfg$ctx_key]] <- opened[[cfg$id]][[1]]
    if (!is.null(cfg$on_open)) {
      child_ctx <- cfg$on_open(opened, child_ctx)
    }
    stack[[length(stack) + 1L]] <- list(level = cfg$child, ctx = child_ctx)
  }
}

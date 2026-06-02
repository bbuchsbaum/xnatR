#' Command-line interface for xnatR
#'
#' Parses xnatR command-line arguments, dispatches to package functions, writes
#' results to stdout, and returns a process-style integer status code.
#'
#' @param args Character vector of command-line arguments.
#'
#' @return Integer status code: `0` for success and `2` for usage/runtime errors.
#' @export
xnatR_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  tryCatch(
    {
      .xnatR_cli_dispatch(args)
    },
    xnatR_cli_usage_error = function(e) {
      message(conditionMessage(e))
      2L
    },
    error = function(e) {
      message("xnatR: ", conditionMessage(e))
      2L
    }
  )
}

#' Install the xnatR command-line wrapper
#'
#' Copies the packaged `xnatR` command wrapper to a directory on your shell
#' `PATH`, or to another destination directory you choose.
#'
#' @param dest_dir Destination directory for command wrappers.
#' @param overwrite If `TRUE`, replace an existing command wrapper.
#' @param commands Optional command names to install. Currently only `"xnatR"`
#'   is supported.
#'
#' @return Invisibly returns the installed wrapper path.
#' @export
install_cli <- function(dest_dir = "~/.local/bin", overwrite = FALSE, commands = NULL) {
  command_map <- .xnatR_cli_command_map()

  if (is.null(commands)) {
    commands <- names(command_map)
  }

  unknown <- setdiff(commands, names(command_map))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown command{?s}: {.val {unknown}}.")
  }

  exec_dir <- system.file("exec", package = "xnatR")
  if (!nzchar(exec_dir)) {
    source_exec <- file.path(getwd(), "exec")
    if (file.exists(file.path(source_exec, "xnatR"))) {
      exec_dir <- source_exec
    }
  }
  if (!nzchar(exec_dir) || !dir.exists(exec_dir)) {
    cli::cli_abort("Could not locate packaged CLI wrappers.")
  }

  dest_dir <- path.expand(dest_dir)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(dest_dir)) {
    cli::cli_abort("Could not create destination directory {.file {dest_dir}}.")
  }

  installed <- character(length(commands))
  names(installed) <- commands

  for (command in commands) {
    source <- file.path(exec_dir, command_map[[command]])
    dest <- file.path(dest_dir, command)

    if (!file.exists(source)) {
      cli::cli_abort("Packaged command wrapper not found: {.file {source}}.")
    }
    if (file.exists(dest) && !isTRUE(overwrite)) {
      cli::cli_abort(c(
        "Command already exists: {.file {dest}}.",
        "i" = "Use {.code overwrite = TRUE} to replace it."
      ))
    }

    ok <- file.copy(source, dest, overwrite = isTRUE(overwrite))
    if (!ok) {
      cli::cli_abort("Failed to install command wrapper to {.file {dest}}.")
    }
    Sys.chmod(dest, mode = "0755")
    installed[[command]] <- dest
  }

  path_entries <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  if (!normalizePath(dest_dir, mustWork = FALSE) %in% normalizePath(path_entries, mustWork = FALSE)) {
    cli::cli_alert_info("{.file {dest_dir}} is not currently on PATH.")
  }

  invisible(installed)
}

.xnatR_cli_command_map <- function() {
  c(xnatR = "xnatR")
}

.xnatR_cli_dispatch <- function(args) {
  if (length(args) > 0 && identical(args[[1]], "--")) {
    args <- args[-1]
  }

  if (length(args) == 0 || identical(args, "--help") || identical(args, "-h")) {
    cat(.xnatR_cli_help(), "\n", sep = "")
    return(0L)
  }

  if (args[[1]] == "help") {
    topic <- if (length(args) >= 2) args[[2]] else NULL
    cat(.xnatR_cli_help(topic), "\n", sep = "")
    return(0L)
  }

  command <- args[[1]]
  command_args <- args[-1]

  if (!command %in% .xnatR_cli_commands()) {
    .xnatR_cli_usage("Unknown command: ", command)
  }

  if ("--help" %in% command_args || "-h" %in% command_args) {
    cat(.xnatR_cli_help(command), "\n", sep = "")
    return(0L)
  }

  parsed <- .xnatR_cli_parse(command_args)
  opts <- parsed$options

  client <- .xnatR_cli_client(opts)
  result <- .xnatR_cli_run(command, opts, client)
  .xnatR_cli_write_result(result, json = isTRUE(opts$json))
  0L
}

.xnatR_cli_commands <- function() {
  c(
    "list-projects",
    "list-subjects",
    "list-experiments",
    "recent-sessions",
    "list-scans",
    "list-resources",
    "list-files",
    "dicom-dump",
    "download-experiment"
  )
}

.xnatR_cli_parse <- function(args) {
  value_options <- c(
    "host", "user", "password", "jsession",
    "columns", "limit", "offset",
    "project", "subject", "experiment", "scan", "resource", "fields",
    "n", "format", "dest-dir", "dest-file"
  )
  flag_options <- c(
    "json", "use-jsession", "ssl-no-verify", "verify",
    "extract", "no-progress", "no-strict"
  )

  opts <- list()
  positionals <- character()
  i <- 1L

  while (i <= length(args)) {
    arg <- args[[i]]

    if (!startsWith(arg, "--")) {
      positionals <- c(positionals, arg)
      i <- i + 1L
      next
    }

    raw <- substring(arg, 3L)
    if (!nzchar(raw)) {
      .xnatR_cli_usage("Invalid option: ", arg)
    }

    if (grepl("=", raw, fixed = TRUE)) {
      parts <- strsplit(raw, "=", fixed = TRUE)[[1]]
      name <- parts[[1]]
      value <- paste(parts[-1], collapse = "=")
    } else {
      name <- raw
      value <- NULL
    }

    if (name %in% flag_options) {
      if (!is.null(value)) {
        .xnatR_cli_usage("Option --", name, " does not take a value.")
      }
      opts[[.xnatR_cli_option_name(name)]] <- TRUE
      i <- i + 1L
      next
    }

    if (name %in% value_options) {
      if (is.null(value)) {
        if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
          .xnatR_cli_usage("Missing value for --", name, ".")
        }
        value <- args[[i + 1L]]
        i <- i + 1L
      }
      opts[[.xnatR_cli_option_name(name)]] <- value
      i <- i + 1L
      next
    }

    .xnatR_cli_usage("Unknown option: --", name)
  }

  list(options = opts, positionals = positionals)
}

.xnatR_cli_option_name <- function(x) {
  gsub("-", "_", x, fixed = TRUE)
}

.xnatR_cli_client <- function(opts) {
  xnat_connect(
    base_url = opts$host %||% NULL,
    username = opts$user %||% NULL,
    password = opts$password %||% NULL,
    ssl_verify = !isTRUE(opts$ssl_no_verify),
    verify = isTRUE(opts$verify),
    use_jsession = isTRUE(opts$use_jsession),
    jsession = opts$jsession %||% NULL
  )
}

.xnatR_cli_run <- function(command, opts, client) {
  switch(command,
    "list-projects" = list_projects(
      columns = .xnatR_cli_columns(opts$columns %||% NULL),
      limit = .xnatR_cli_integer(opts$limit %||% NULL, "limit"),
      offset = .xnatR_cli_integer(opts$offset %||% NULL, "offset"),
      client = client
    ),
    "list-subjects" = list_subjects(
      project_id = .xnatR_cli_required(opts$project, "project"),
      columns = .xnatR_cli_columns(opts$columns %||% NULL),
      limit = .xnatR_cli_integer(opts$limit %||% NULL, "limit"),
      offset = .xnatR_cli_integer(opts$offset %||% NULL, "offset"),
      client = client
    ),
    "list-experiments" = list_experiments(
      project_id = .xnatR_cli_required(opts$project, "project"),
      subject_id = opts$subject %||% NULL,
      columns = .xnatR_cli_columns(opts$columns %||% NULL),
      limit = .xnatR_cli_integer(opts$limit %||% NULL, "limit"),
      offset = .xnatR_cli_integer(opts$offset %||% NULL, "offset"),
      client = client
    ),
    "recent-sessions" = list_recent_sessions(
      project_id = .xnatR_cli_required(opts$project, "project"),
      n = .xnatR_cli_integer(opts$n %||% NULL, "n"),
      subject_id = opts$subject %||% NULL,
      columns = .xnatR_cli_columns(opts$columns %||% NULL),
      client = client
    ),
    "list-scans" = list_scans(
      project_id = .xnatR_cli_required(opts$project, "project"),
      subject_id = .xnatR_cli_required(opts$subject, "subject"),
      experiment_id = .xnatR_cli_required(opts$experiment, "experiment"),
      columns = .xnatR_cli_columns(opts$columns %||% NULL),
      limit = .xnatR_cli_integer(opts$limit %||% NULL, "limit"),
      offset = .xnatR_cli_integer(opts$offset %||% NULL, "offset"),
      client = client
    ),
    "list-resources" = list_resources(
      project_id = .xnatR_cli_required(opts$project, "project"),
      subject_id = .xnatR_cli_required(opts$subject, "subject"),
      experiment_id = .xnatR_cli_required(opts$experiment, "experiment"),
      scan_id = .xnatR_cli_required(opts$scan, "scan"),
      client = client
    ),
    "list-files" = list_files(
      project_id = .xnatR_cli_required(opts$project, "project"),
      subject_id = .xnatR_cli_required(opts$subject, "subject"),
      experiment_id = .xnatR_cli_required(opts$experiment, "experiment"),
      scan_id = .xnatR_cli_required(opts$scan, "scan"),
      resource = .xnatR_cli_required(opts$resource, "resource"),
      client = client
    ),
    "dicom-dump" = dicom_dump(
      experiment_id = .xnatR_cli_required(opts$experiment, "experiment"),
      scan_id = opts$scan %||% NULL,
      fields = .xnatR_cli_columns(opts$fields %||% NULL),
      client = client
    ),
    "download-experiment" = download_experiment(
      experiment_id = .xnatR_cli_required(opts$experiment, "experiment"),
      scan_id = opts$scan %||% "ALL",
      format = opts$format %||% "zip",
      dest_dir = opts$dest_dir %||% tempdir(),
      dest_file = opts$dest_file %||% NULL,
      extract = isTRUE(opts$extract),
      progress = !isTRUE(opts$no_progress),
      strict = !isTRUE(opts$no_strict),
      client = client
    ),
    .xnatR_cli_usage("Unknown command: ", command)
  )
}

.xnatR_cli_write_result <- function(result, json = FALSE) {
  if (isTRUE(json)) {
    if (is.data.frame(result)) {
      payload <- as.data.frame(result)
    } else if (is.null(result)) {
      payload <- list(path = NULL)
    } else if (is.character(result) && length(result) > 1) {
      payload <- list(paths = unname(result))
    } else if (is.character(result) && length(result) == 1) {
      payload <- list(path = unname(result))
    } else {
      payload <- result
    }
    cat(jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE, na = "null", pretty = TRUE), "\n")
    return(invisible(NULL))
  }

  if (is.data.frame(result)) {
    print(result)
  } else if (!is.null(result)) {
    cat(paste(result, collapse = "\n"), "\n", sep = "")
  }
  invisible(NULL)
}

.xnatR_cli_columns <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(NULL)
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

.xnatR_cli_integer <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  value <- suppressWarnings(as.integer(x))
  if (is.na(value) || !identical(as.character(value), as.character(x))) {
    .xnatR_cli_usage("Option --", name, " must be an integer.")
  }
  value
}

.xnatR_cli_required <- function(x, name) {
  if (is.null(x) || !nzchar(x)) {
    .xnatR_cli_usage("Missing required option --", name, ".")
  }
  x
}

.xnatR_cli_usage <- function(...) {
  msg <- paste0(...)
  stop(structure(list(message = msg, call = NULL), class = c("xnatR_cli_usage_error", "error", "condition")))
}

.xnatR_cli_help <- function(topic = NULL) {
  if (is.null(topic)) {
    return(paste(c(
      "Usage: xnatR <command> [options]",
      "",
      "Commands:",
      "  list-projects       List projects",
      "  list-subjects       List subjects in a project",
      "  list-experiments    List experiments in a project or subject",
      "  recent-sessions     List recent sessions for a project",
      "  list-scans          List scans in an experiment",
      "  list-resources      List resources for a scan",
      "  list-files          List files for a scan resource",
      "  dicom-dump          Dump DICOM header fields for a session or scan",
      "  download-experiment Download an experiment archive",
      "",
      "Common options:",
      "  --host URL          XNAT server URL, or use XNATR_HOST",
      "  --user USER         XNAT username, or use XNATR_USER",
      "  --password PASS     XNAT password, or use XNATR_PASS",
      "  --jsession ID       Existing JSESSIONID",
      "  --use-jsession      Establish a JSESSION before running the command",
      "  --ssl-no-verify     Disable SSL certificate verification",
      "  --verify            Verify credentials before running the command",
      "  --json              Write JSON output",
      "",
      "Run `xnatR help <command>` for command-specific options."
    ), collapse = "\n"))
  }

  switch(topic,
    "list-projects" = paste(c(
      "Usage: xnatR list-projects [--columns COLS] [--limit N] [--offset N] [--json]",
      "",
      "Example:",
      "  xnatR list-projects --host https://central.xnat.org --user guest --password guest"
    ), collapse = "\n"),
    "list-subjects" = paste(c(
      "Usage: xnatR list-subjects --project PROJECT [--columns COLS] [--limit N] [--offset N] [--json]",
      "",
      "Example:",
      "  xnatR list-subjects --project CENTRAL_OASIS --json"
    ), collapse = "\n"),
    "list-experiments" = paste(c(
      "Usage: xnatR list-experiments --project PROJECT [--subject SUBJECT] [--columns COLS] [--limit N] [--offset N] [--json]",
      "",
      "Example:",
      "  xnatR list-experiments --project CENTRAL_OASIS --subject OAS1_0001"
    ), collapse = "\n"),
    "recent-sessions" = paste(c(
      "Usage: xnatR recent-sessions --project PROJECT [--subject SUBJECT] [--n N] [--columns COLS] [--json]",
      "",
      "Example:",
      "  xnatR recent-sessions --project CENTRAL_OASIS --n 10"
    ), collapse = "\n"),
    "list-scans" = paste(c(
      "Usage: xnatR list-scans --project PROJECT --subject SUBJECT --experiment EXPERIMENT [--columns COLS] [--limit N] [--offset N] [--json]",
      "",
      "Example:",
      "  xnatR list-scans --project CENTRAL_OASIS --subject OAS1_0001 --experiment OAS1_0001_MR1"
    ), collapse = "\n"),
    "list-resources" = paste(c(
      "Usage: xnatR list-resources --project PROJECT --subject SUBJECT --experiment EXPERIMENT --scan SCAN [--json]",
      "",
      "Example:",
      "  xnatR list-resources --project CENTRAL_OASIS --subject OAS1_0001 --experiment OAS1_0001_MR1 --scan 1"
    ), collapse = "\n"),
    "list-files" = paste(c(
      "Usage: xnatR list-files --project PROJECT --subject SUBJECT --experiment EXPERIMENT --scan SCAN --resource RESOURCE [--json]",
      "",
      "Example:",
      "  xnatR list-files --project CENTRAL_OASIS --subject OAS1_0001 --experiment OAS1_0001_MR1 --scan 1 --resource DICOM"
    ), collapse = "\n"),
    "dicom-dump" = paste(c(
      "Usage: xnatR dicom-dump --experiment EXPERIMENT [--scan SCAN] [--fields TAGS] [--json]",
      "",
      "TAGS is a comma-separated list of DICOM tags, e.g. 00080060,00180080",
      "(also accepts (0008,0060) form). Omit --fields to dump every field.",
      "",
      "Example:",
      "  xnatR dicom-dump --experiment OAS1_0001_MR1 --scan 1 --fields 00080060,00180080"
    ), collapse = "\n"),
    "download-experiment" = paste(c(
      "Usage: xnatR download-experiment --experiment EXPERIMENT [--scan SCAN] [--format zip|tar.gz] [--dest-dir DIR] [--dest-file FILE] [--extract] [--no-progress] [--no-strict] [--json]",
      "",
      "Example:",
      "  xnatR download-experiment --experiment OAS1_0001_MR1 --dest-dir downloads --no-progress"
    ), collapse = "\n"),
    .xnatR_cli_usage("Unknown help topic: ", topic)
  )
}

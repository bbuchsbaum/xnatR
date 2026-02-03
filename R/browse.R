#' Interactive browsing helpers
#'
#' These helpers provide an interactive console UI for exploring XNAT resources.
#' They are designed to be safe in non-interactive contexts: when
#' `.interactive = FALSE`, the functions return the corresponding listing tibble
#' without prompting.
#'
#' @name xnat_browse
NULL

.browse_help <- function() {
  cli::cli_text("Commands:")
  cli::cli_ul(c(
    "{.code <n>} or {.code <n,m>} or {.code <n-m>} to select rows",
    "{.code d <n>} to show details for a row",
    "{.code /<text>} to filter the current page (case-insensitive)",
    "{.code n} next page",
    "{.code p} previous page",
    "{.code ?} help",
    "{.code q} quit"
  ))
}

.browse_parse_selection <- function(spec, max_index) {
  spec <- trimws(spec)
  if (!nzchar(spec)) return(integer())
  if (tolower(spec) == "all") return(seq_len(max_index))

  parts <- strsplit(spec, ",", fixed = TRUE)[[1]]
  indices <- integer()

  for (part in parts) {
    part <- trimws(part)
    if (!nzchar(part)) next

    if (grepl("-", part, fixed = TRUE)) {
      range_parts <- strsplit(part, "-", fixed = TRUE)[[1]]
      if (length(range_parts) != 2) {
        cli::cli_abort("Invalid selection: {.val {part}}")
      }
      start <- suppressWarnings(as.integer(trimws(range_parts[1])))
      end <- suppressWarnings(as.integer(trimws(range_parts[2])))
      if (is.na(start) || is.na(end)) {
        cli::cli_abort("Invalid selection: {.val {part}}")
      }
      if (start > end) {
        cli::cli_abort("Invalid range (start > end): {.val {part}}")
      }
      indices <- c(indices, seq.int(start, end))
      next
    }

    value <- suppressWarnings(as.integer(part))
    if (is.na(value)) {
      cli::cli_abort("Invalid selection: {.val {part}}")
    }
    indices <- c(indices, value)
  }

  indices <- unique(indices)
  indices <- indices[order(indices)]

  if (length(indices) == 0) return(indices)
  if (any(indices < 1L) || any(indices > max_index)) {
    cli::cli_abort("Selection out of range: 1-{max_index}.")
  }

  indices
}

.browse_print_table <- function(x, columns, title = NULL, subtitle = NULL) {
  if (!is.null(title)) {
    cli::cli_h1(title)
  }
  if (!is.null(subtitle) && nzchar(subtitle)) {
    cli::cli_text(cli::col_grey(subtitle))
  }

  if (nrow(x) == 0) {
    cli::cli_alert_warning("No results.")
    return(invisible(NULL))
  }

  display_cols <- intersect(columns, names(x))
  if (length(display_cols) == 0) {
    display_cols <- names(x)
  }

  idx <- seq_len(nrow(x))
  display <- tibble::as_tibble(x[, display_cols, drop = FALSE])
  display <- tibble::add_column(display, .i = idx, .before = 1)
  print(display)
  invisible(NULL)
}

.browse_loop <- function(fetch_page,
                         title,
                         columns,
                         page_size = 20,
                         filter = NULL,
                         input_fn = readline) {
  page <- 0L
  pages <- list()

  get_page <- function(page_num) {
    key <- as.character(page_num)
    if (!is.null(pages[[key]])) return(pages[[key]])
    offset <- page_num * page_size
    data <- fetch_page(limit = page_size, offset = offset)
    pages[[key]] <<- data
    data
  }

  current_filter <- filter

  repeat {
    data <- get_page(page)

    view <- data
    if (!is.null(current_filter) && nzchar(current_filter) && nrow(view) > 0) {
      # Filter on displayable columns by substring match.
      filter_cols <- intersect(columns, names(view))
      if (length(filter_cols) == 0) filter_cols <- names(view)
      haystack <- do.call(
        paste,
        c(lapply(view[, filter_cols, drop = FALSE], as.character), sep = " | ")
      )
      keep <- grepl(current_filter, haystack, ignore.case = TRUE)
      view <- view[keep, , drop = FALSE]
    }

    subtitle <- NULL
    if (!is.null(current_filter) && nzchar(current_filter)) {
      subtitle <- paste0("Filter: ", current_filter)
    }
    .browse_print_table(view, columns = columns, title = title, subtitle = subtitle)
    cli::cli_text(cli::col_grey("Page {page + 1} (n/p), filter with /text, select rows, q to quit."))

    cmd <- input_fn("> ")
    cmd <- trimws(cmd)
    if (!nzchar(cmd)) next

    if (tolower(cmd) %in% c("q", "quit")) {
      return(NULL)
    }
    if (cmd == "?") {
      .browse_help()
      next
    }
    if (tolower(cmd) == "n") {
      if (nrow(data) < page_size) {
        cli::cli_alert_info("End of results.")
      } else {
        page <- page + 1L
      }
      next
    }
    if (tolower(cmd) == "p") {
      if (page == 0L) {
        cli::cli_alert_info("Already at first page.")
      } else {
        page <- page - 1L
      }
      next
    }
    if (startsWith(cmd, "/")) {
      current_filter <- substring(cmd, 2)
      next
    }
    if (startsWith(tolower(cmd), "d ")) {
      i <- suppressWarnings(as.integer(trimws(substring(cmd, 3))))
      if (is.na(i) || i < 1L || i > nrow(view)) {
        cli::cli_alert_warning("Invalid row index.")
        next
      }
      cli::cli_h2("Row {i}")
      print(tibble::as_tibble(view[i, , drop = FALSE]))
      next
    }

    # Selection
    tryCatch({
      idx <- .browse_parse_selection(cmd, nrow(view))
      if (length(idx) == 0) {
        cli::cli_alert_warning("No selection.")
        next
      }
      return(view[idx, , drop = FALSE])
    }, error = function(e) {
      cli::cli_alert_warning(conditionMessage(e))
      NULL
    })
  }
}

#' Browse projects interactively
#'
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @param pattern Optional substring filter applied to the current page.
#' @param columns Columns to display (defaults to common project fields).
#' @param page_size Rows per page (default 20).
#' @param .interactive If `FALSE`, returns the listing tibble without prompting.
#'
#' @return A tibble row selection (or `NULL` if quit). In non-interactive mode,
#'   returns a tibble of projects.
#' @export
xnat_browse_projects <- function(client = NULL,
                                pattern = NULL,
                                columns = c("ID", "name", "description"),
                                page_size = 20,
                                .interactive = interactive()) {
  if (!.interactive) {
    return(list_projects(columns = columns, client = client))
  }

  .browse_loop(
    fetch_page = function(limit, offset) list_projects(columns = columns, limit = limit, offset = offset, client = client),
    title = "XNAT Projects",
    columns = columns,
    page_size = page_size,
    filter = pattern
  )
}

#' Browse subjects interactively
#'
#' If `project_id` is omitted in interactive mode, the function first prompts for
#' a project.
#'
#' @param project_id Project identifier (optional in interactive mode).
#' @param client Optional `xnat_client`. If `NULL`, uses the global session.
#' @param pattern Optional substring filter applied to the current page.
#' @param columns Columns to display (defaults to common subject fields).
#' @param page_size Rows per page (default 20).
#' @param .interactive If `FALSE`, returns the listing tibble without prompting.
#'
#' @return A tibble row selection (or `NULL` if quit). In non-interactive mode,
#'   returns a tibble of subjects.
#' @export
xnat_browse_subjects <- function(project_id = NULL,
                                client = NULL,
                                pattern = NULL,
                                columns = c("ID", "label", "gender", "age"),
                                page_size = 20,
                                .interactive = interactive()) {
  if (!.interactive) {
    check_string(project_id, "project_id")
    return(list_subjects(project_id = project_id, columns = columns, client = client))
  }

  if (is.null(project_id)) {
    proj <- xnat_browse_projects(client = client, .interactive = TRUE)
    if (is.null(proj) || nrow(proj) == 0) return(NULL)
    if (!("ID" %in% names(proj))) {
      cli::cli_abort("Selected project does not include an {.field ID} column.")
    }
    project_id <- proj$ID[[1]]
  }

  .browse_loop(
    fetch_page = function(limit, offset) list_subjects(project_id = project_id, columns = columns, limit = limit, offset = offset, client = client),
    title = paste0("XNAT Subjects (", project_id, ")"),
    columns = columns,
    page_size = page_size,
    filter = pattern
  )
}

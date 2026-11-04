# Strict collection traversal used by inventory/planning. Legacy list_* calls
# keep their existing single-request/explicit-limit semantics.
inventory_abort <- function(message, class = "xnatR_inventory_error") {
  rlang::abort(message, class = class)
}

inventory_text <- function(x, name) {
  if (!name %in% names(x)) return(rep(NA_character_, nrow(x)))
  values <- x[[name]]
  if (is.numeric(values) && any(abs(values) >= 2^53, na.rm = TRUE)) {
    inventory_abort(paste0("Inexact numeric identifiers in ", name, "."))
  }
  if (is.list(values)) inventory_abort(paste0("Expected scalar values in ", name, "."))
  values <- if (is.numeric(values)) {
    vapply(values, function(value) if (is.na(value)) NA_character_ else
      format(value, scientific = FALSE, trim = TRUE, digits = 22), character(1))
  } else as.character(values)
  values[!is.na(values) & !nzchar(values)] <- NA_character_
  values
}

inventory_ids <- function(x, key) {
  ids <- inventory_text(x, key)
  if (anyNA(ids) || anyDuplicated(ids)) {
    inventory_abort(paste0("Missing or duplicate canonical ", key, " values."))
  }
  ids
}

inventory_bind <- function(tables, template = tibble::tibble()) {
  tables <- Filter(function(x) nrow(x) > 0, tables)
  if (!length(tables)) return(template)
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  tables <- lapply(tables, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA_character_
    x[columns]
  })
  tibble::as_tibble(do.call(rbind, tables))
}

inventory_total <- function(json) {
  total <- json$ResultSet$totalRecords
  if (is.null(total)) return(NULL)
  if (length(total) != 1 || is.na(total) || !grepl("^[0-9]+$", as.character(total))) {
    inventory_abort("Invalid totalRecords in collection response.")
  }
  total <- as.numeric(total)
  if (!is.finite(total) || total >= 2^53) inventory_abort("Invalid totalRecords.")
  total
}

inventory_page <- function(path, query, key, client) {
  json <- xnat_get(path, query = query, client = client)
  rows <- parse_xnat_result(json)
  # Canonical IDs are always strings, even when a server emits JSON numbers.
  if (nrow(rows)) rows[[key]] <- inventory_ids(rows, key)
  list(rows = rows, total = inventory_total(json))
}

xnat_collect <- function(path, key = "ID", client = NULL, page_size = 100L,
                         pagination = "auto", max_pages = 10000L, query = list()) {
  if (length(page_size) != 1 || !is.numeric(page_size) || is.na(page_size) || !is.finite(page_size) ||
      page_size < 1 || page_size != floor(page_size)) inventory_abort("page_size must be a positive integer.")
  if (length(max_pages) != 1 || !is.numeric(max_pages) || is.na(max_pages) || !is.finite(max_pages) || max_pages > .Machine$integer.max ||
      max_pages < 1 || max_pages != floor(max_pages)) inventory_abort("max_pages must be a positive integer.")
  pagination <- match.arg(pagination, c("auto", "paged", "all"))
  finish <- function(rows, mode, requests) {
    attr(rows, "pagination") <- list(path = path, mode = mode, requests = requests, rows = nrow(rows))
    rows
  }
  all_rows <- function(previous = NULL, requests = 0L) {
    result <- inventory_page(path, utils::modifyList(query, list(limit = "*", offset = 0)), key, client)
    if (is.null(result$total) || result$total != nrow(result$rows)) {
      inventory_abort("Cannot verify complete limit=* response: totalRecords is missing or mismatched.")
    }
    if (!is.null(previous) && nrow(previous)) {
      idx <- match(previous[[key]], result$rows[[key]])
      if (anyNA(idx) || !isTRUE(all.equal(as.data.frame(previous),
          as.data.frame(result$rows[idx, names(previous), drop = FALSE]), check.attributes = FALSE))) {
        inventory_abort("Collection changed during pagination; refresh the inventory.")
      }
    }
    finish(result$rows, "all", requests + 1L)
  }
  if (pagination == "all") return(all_rows())
  pages <- list()
  seen <- character()
  totals <- numeric()
  offset <- 0
  for (i in seq_len(max_pages)) {
    page <- inventory_page(path, utils::modifyList(query, list(limit = page_size, offset = offset)), key, client)
    rows <- page$rows
    if (!is.null(page$total)) totals <- c(totals, page$total)
    if (length(unique(totals)) > 1L) inventory_abort("totalRecords changed during pagination.")
    if (!nrow(rows)) {
      result <- inventory_bind(pages)
      if (length(totals) && any(totals != nrow(result))) inventory_abort("Pagination ended before totalRecords was satisfied.")
      return(finish(result, "paged", i))
    }
    if (nrow(rows) > page_size || any(rows[[key]] %in% seen)) {
      repeated_first <- length(pages) == 1L && identical(rows, pages[[1]])
      if (pagination == "auto" && (i == 1L || repeated_first)) {
        return(all_rows(if (i == 1L) rows else pages[[1]], i))
      }
      inventory_abort("Overlapping pages or ignored pagination; cannot establish completeness.")
    }
    pages[[i]] <- rows
    seen <- c(seen, rows[[key]])
    offset <- offset + nrow(rows)
    # A short page is not proof of exhaustion: a server may cap page sizes.
  }
  inventory_abort("Pagination reached max_pages without proving completeness.")
}

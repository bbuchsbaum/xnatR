# Common execution boundary. Requests remain httr2 objects for URL/body building.
xnat_perform <- function(req, client = NULL, path = NULL, renew = TRUE,
                         backend = NULL, check_status = TRUE) {
  owner <- client %||% xnatR_env
  backend <- backend %||% owner$backend %||% "httr2"
  perform <- function(req) {
    if (identical(backend, "curl")) return(system_curl_perform(req, path))
    req <- httr2::req_options(req, followlocation = FALSE) |>
      httr2::req_error(is_error = function(resp) FALSE)
    tryCatch(httr2::req_perform(req, path = path), error = function(e) {
      category <- if (grepl("certificate|SSL|TLS", conditionMessage(e), ignore.case = TRUE)) "certificate" else "network"
      transport_abort(category)
    })
  }
  resp <- perform(req)
  status <- httr2::resp_status(resp)
  if (status == 401L && renew && !is.null(owner$session) &&
      !is.null(owner$credential_callback) &&
      (req$method %||% "GET") %in% c("GET", "HEAD") &&
      !is.null(req$headers$Cookie)) {
    creds <- request_credentials(owner$credential_callback)
    owner$session$jsession <- establish_jsession(owner$base_url, creds$username,
      creds$password, owner$ssl_verify, backend = backend)
    req <- httr2::req_headers(req, Cookie = paste0("JSESSIONID=", owner$session$jsession))
    resp <- perform(req)
    status <- httr2::resp_status(resp)
  }
  if (check_status && status >= 300L) {
    category <- if (status == 401L) "authentication" else if (status == 403L) "permission" else if (status < 400L) "redirect" else "http"
    transport_abort(category, status)
  }
  resp
}

transport_abort <- function(category, status = NULL) {
  rlang::abort(paste0("XNAT ", category, " failure",
    if (!is.null(status)) paste0(" (HTTP ", status, ")"), "."),
    class = c(paste0("xnatR_", category, "_error"), "xnatR_transport_error"),
    status = status)
}

curl_config_quote <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  paste0('"', x, '"')
}

system_curl_perform <- function(req, path = NULL) {
  executable <- if (file.exists("/usr/bin/curl")) "/usr/bin/curl" else Sys.which("curl")
  if (!nzchar(executable)) rlang::abort("System curl is unavailable.")
  scratch <- tempfile("xnatr-")
  dir.create(scratch, mode = "0700")
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  body_path <- path %||% file.path(scratch, "body")
  headers_path <- file.path(scratch, "headers")
  timeout <- if (!is.null(req$options$timeout_ms)) req$options$timeout_ms / 1000 else req$options$timeout %||% 300
  cfg <- c("silent", "show-error", "globoff", 'proto = "=http,https"',
    paste0("url = ", curl_config_quote(req$url)),
    paste0("request = ", curl_config_quote(req$method %||% if (is.null(req$body)) "GET" else "POST")),
    paste0("output = ", curl_config_quote(body_path)),
    paste0("dump-header = ", curl_config_quote(headers_path)),
    'write-out = "%{http_code}"', "connect-timeout = 30",
    paste0("max-time = ", timeout))
  if (identical(req$options$ssl_verifypeer, FALSE)) cfg <- c(cfg, "insecure")
  if (!is.null(req$xnatr_credentials)) {
    cfg <- c(cfg, paste0("user = ", curl_config_quote(paste0(
      req$xnatr_credentials$username, ":", req$xnatr_credentials$password))))
  }
  for (name in names(req$headers)) {
    if (tolower(name) == "authorization" && !is.null(req$xnatr_credentials)) next
    cfg <- c(cfg, paste0("header = ", curl_config_quote(paste0(name, ": ", req$headers[[name]]))))
  }
  if (!is.null(req$body)) {
    body <- req$body
    data <- if (body$type == "json") {
      do.call(jsonlite::toJSON, c(list(x = body$data), body$params))
    } else if (body$type == "raw") {
      if (is.raw(body$data)) rawToChar(body$data) else body$data
    } else rlang::abort("Unsupported request body for system curl.")
    cfg <- c(cfg, paste0("header = ", curl_config_quote(paste0("Content-Type: ", body$content_type))),
      paste0("data-binary = ", curl_config_quote(data)))
  }
  # Disable .curlrc; secrets exist only in memory and the child's stdin pipe.
  proc <- processx::process$new(executable, c("-q", "--config", "-"),
    stdin = "|", stdout = "|", stderr = "|")
  on.exit(if (proc$is_alive()) proc$kill(), add = TRUE)
  proc$write_input(paste0(paste(cfg, collapse = "\n"), "\n"))
  processx::processx_conn_close(proc$get_input_connection())
  proc$wait(timeout = (timeout + 5) * 1000)
  if (proc$is_alive()) transport_abort("network")
  code <- proc$get_exit_status()
  if (code != 0L) transport_abort(if (code %in% c(51L, 58L, 60L, 77L, 83L, 90L, 91L)) "certificate" else "network")
  status <- as.integer(proc$read_all_output())
  lines <- readLines(headers_path, warn = FALSE)
  starts <- which(grepl("^HTTP/", lines))
  if (length(starts)) lines <- lines[seq.int(utils::tail(starts, 1) + 1L, length(lines))]
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  headers <- as.list(trimws(sub("^[^:]+:", "", lines)))
  names(headers) <- tolower(sub(":.*", "", lines))
  body <- if (is.null(path)) readBin(body_path, "raw", n = file.info(body_path)$size) else raw()
  httr2::response(status_code = status, url = req$url,
    method = req$method %||% "GET", headers = headers, body = body)
}

#' Diagnose connectivity without credentials
#' @param base_url XNAT server URL.
#' @param backend Either `httr2` or `curl`.
#' @param timeout Maximum request duration in seconds.
#' @return A list with reachability, HTTP status and failure category. HTTP 401
#'   establishes reachability, not authenticated access. TLS is always verified.
#' @export
xnat_diagnose <- function(base_url, backend = c("httr2", "curl"), timeout = 30) {
  backend <- match.arg(backend)
  validate_server_url(base_url)
  req <- httr2::request(sub("/+$", "", base_url)) |>
    httr2::req_url_path_append("data/projects") |>
    httr2::req_timeout(timeout)
  tryCatch({
    resp <- xnat_perform(req, backend = backend, check_status = FALSE, renew = FALSE)
    status <- httr2::resp_status(resp)
    list(backend = backend, reachable = TRUE, status = status,
      category = if (status == 401) "authentication" else if (status == 403) "permission" else if (status >= 300 && status < 400) "redirect" else if (status >= 400) "http" else "ok")
  }, xnatR_transport_error = function(e) {
    list(backend = backend, reachable = FALSE, status = NA_integer_,
      category = sub("^xnatR_(.*)_error$", "\\1", class(e)[1]))
  })
}

validate_server_url <- function(url) {
  check_string(url, "base_url")
  parsed <- httr2::url_parse(url)
  if (!parsed$scheme %in% c("http", "https") || !nzchar(parsed$hostname %||% "") ||
      !is.null(parsed$username) || !is.null(parsed$password) ||
      !is.null(parsed$query) || !is.null(parsed$fragment)) {
    rlang::abort("Use an HTTP(S) server URL without credentials, query, or fragment.")
  }
}

request_credentials <- function(callback) {
  value <- tryCatch(callback(), interrupt = function(e) NULL)
  if (is.null(value)) rlang::abort("XNAT authentication cancelled.", class = "xnatR_cancelled")
  check_string(value$username, "username")
  check_string(value$password, "password")
  value
}

#' Prompt for XNAT credentials in a terminal
#' @return A username/password list, or NULL on cancellation. Requires getPass.
#' @export
xnat_credentials <- function() {
  if (!interactive()) rlang::abort("Supply a credential callback in noninteractive R.")
  if (!requireNamespace("getPass", quietly = TRUE)) rlang::abort("Install getPass for hidden terminal password entry, or supply a credential callback.")
  username <- readline("XNAT username (empty to cancel): ")
  if (!nzchar(username)) return(NULL)
  password <- getPass::getPass("XNAT password: ")
  if (is.null(password) || !nzchar(password)) return(NULL)
  list(username = username, password = password)
}

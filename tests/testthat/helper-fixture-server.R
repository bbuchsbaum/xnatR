# Start a local Python fixture server and return its process and base URL.
# The server writes its port to `ready` once it is listening. Cold interpreter
# start-up on CI runners can take several seconds, so wait up to `timeout`
# seconds, stop early if the process exits, and fail with its stderr instead of
# a bare "cannot open file" when the port never appears.
start_fixture_server <- function(script, args = character(), ready = tempfile(), timeout = 30) {
  python <- Sys.which("python3")
  if (!nzchar(python)) testthat::skip("python3 unavailable")
  errors <- paste0(ready, ".stderr")
  proc <- processx::process$new(python, c(test_path("fixtures", script), ready, args),
    stdout = NULL, stderr = errors, cleanup = TRUE)
  deadline <- Sys.time() + timeout
  repeat {
    if (file.exists(ready) && file.info(ready)$size > 0) {
      port <- trimws(readLines(ready, warn = FALSE))
      if (length(port) == 1L && grepl("^[0-9]+$", port)) break
    }
    if (!proc$is_alive() || Sys.time() > deadline) {
      status <- if (proc$is_alive()) "timed out" else paste("exited with status", proc$get_exit_status())
      proc$kill()
      err <- if (file.exists(errors)) paste(readLines(errors, warn = FALSE), collapse = "\n") else ""
      stop(sprintf("Fixture server %s %s before reporting a port.%s", script, status,
        if (nzchar(err)) paste0("\nstderr:\n", err) else ""), call. = FALSE)
    }
    Sys.sleep(0.05)
  }
  list(proc = proc, url = paste0("http://127.0.0.1:", port, "/xnat"), ready = ready)
}

# Wait for a file written by a helper subprocess, failing with its stderr.
wait_for_file <- function(path, proc, timeout = 30) {
  deadline <- Sys.time() + timeout
  while (!file.exists(path)) {
    if (!proc$is_alive() || Sys.time() > deadline) {
      err <- tryCatch(paste(proc$read_all_error_lines(), collapse = "\n"), error = function(e) "")
      stop(sprintf("Subprocess did not create %s.%s", basename(path),
        if (nzchar(err)) paste0("\nstderr:\n", err) else ""), call. = FALSE)
    }
    Sys.sleep(0.05)
  }
  invisible(TRUE)
}

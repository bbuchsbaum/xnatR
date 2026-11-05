test_that("diagnostics never attach authentication", {
  local_mocked_bindings(xnat_perform = function(req, ...) {
    expect_null(req$headers$Authorization)
    expect_null(req$headers$Cookie)
    httr2::response(401)
  })
  result <- xnat_diagnose("https://example.org")
  expect_true(result$reachable)
  expect_equal(result$category, "authentication")
})

test_that("failed diagnosis precedes callback and cancellation is explicit", {
  called <- FALSE
  local_mocked_bindings(xnat_diagnose = function(...) list(reachable = FALSE, category = "network"))
  expect_error(xnat_connect("https://example.org", credential_callback = function() {
    called <<- TRUE
  }), class = "xnatR_network_error")
  expect_false(called)
  expect_error(request_credentials(function() NULL), class = "xnatR_cancelled")
})

test_that("session renewal happens once for reads and never for permission errors", {
  client <- xnat_client("https://example.org", jsession = "old", backend = "curl")
  calls <- 0L
  renewals <- 0L
  client$credential_callback <- function() list(username = "u", password = "p")
  local_mocked_bindings(
    system_curl_perform = function(req, path) {
      calls <<- calls + 1L
      httr2::response(if (calls == 1L) 401 else 200)
    },
    establish_jsession = function(...) { renewals <<- renewals + 1L; "new" }
  )
  xnat_perform(xnat_request("data/projects", client), client)
  expect_equal(client$session$jsession, "new")
  expect_equal(renewals, 1L)
  expect_equal(xnat_request("data/projects", client)$headers$Cookie, "JSESSIONID=new")
  local_mocked_bindings(system_curl_perform = function(...) httr2::response(401))
  expect_error(xnat_perform(xnat_request("data/projects", client), client), class = "xnatR_authentication_error")
  expect_equal(renewals, 2L)
  local_mocked_bindings(system_curl_perform = function(...) httr2::response(403))
  expect_error(xnat_perform(xnat_request("data/projects", client), client), class = "xnatR_permission_error")
  expect_equal(renewals, 2L)
})

test_that("project-wide sessions without subjects open by canonical experiment ID", {
  levels <- .explore_levels()
  ctx <- levels$experiments$on_open(tibble::tibble(ID = "E1"), list(project_id = "P", experiment_id = "E1"))
  expect_null(ctx$subject_id)
  local_mocked_bindings(xnat_get_tibble = function(path, ...) {
    expect_equal(path, "data/experiments/E1/scans")
    tibble::tibble(ID = "1")
  })
  expect_equal(levels$scans$fetch(ctx, NULL)$ID, "1")
})

test_that("server URLs cannot carry credentials or query secrets", {
  expect_error(xnat_client("https://user:pass@example.org"), "without credentials")
  expect_error(xnat_diagnose("https://example.org?token=secret"), "without credentials")
})

test_that("both backends authenticate, encode queries, POST and stream identical bytes", {
  skip_if(Sys.which("python3") == "", "python3 unavailable")
  ready <- tempfile()
  proc <- processx::process$new(Sys.which("python3"),
    c(test_path("fixtures", "transport-server.py"), ready), stdout = "|", stderr = "|")
  on.exit({ proc$kill(); unlink(ready) }, add = TRUE)
  for (i in seq_len(100)) {
    if (file.exists(ready) && file.info(ready)$size > 0) break
    Sys.sleep(0.05)
  }
  url <- paste0("http://127.0.0.1:", readLines(ready, warn = FALSE), "/xnat")
  for (backend in c("httr2", "curl")) {
    client <- xnat_client(url, username = "test", password = 'quote"slash\\password', backend = backend)
    response <- xnat_get("echo", list(value = "a & b"), client)
    expect_match(response$path, "value=a%20%26%20b", fixed = TRUE)
    expect_match(response$authorization, "^Basic ")
    expect_equal(xnat_post("echo", list(value = 'a"b\\c'), client = client)$value, 'a"b\\c')
    dest <- tempfile()
    xnat_download("archive", dest, progress = FALSE, client = client)
    expect_identical(readBin(dest, "raw", n = 262144), rep(as.raw(0:255), 1024))
    unlink(dest)
    session <- xnat_connect(url, backend = backend, verify = FALSE,
      credential_callback = function() list(username = "u", password = "p"))
    expect_null(session$password)
    expect_equal(xnat_get("echo", client = session)$cookie, "JSESSIONID=synthetic-session")
  }
})

test_that("redirects and mutations never trigger credential renewal", {
  client <- xnat_client("https://example.org", jsession = "old", backend = "curl")
  client$credential_callback <- function() stop("must not prompt")
  local_mocked_bindings(system_curl_perform = function(...) httr2::response(401))
  req <- httr2::req_method(xnat_request("data/search", client), "POST")
  expect_error(xnat_perform(req, client), class = "xnatR_authentication_error")
  local_mocked_bindings(system_curl_perform = function(...) httr2::response(302))
  expect_error(xnat_perform(xnat_request("data/projects", client), client), class = "xnatR_redirect_error")
})

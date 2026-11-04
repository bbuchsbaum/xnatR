# xnatR

An R package for interacting with [XNAT](https://www.xnat.org/) neuroimaging
archives through the REST API.

## Installation

```r
# install.packages("remotes")
remotes::install_github("bbuchsbaum/xnatR")
```

## Quick Start

Prefer an explicit client when writing reusable scripts or command-line jobs:

```r
library(xnatR)

client <- xnat_connect(
  base_url = "https://central.xnat.org",
  username = "guest",
  password = "guest",
  use_jsession = TRUE
)

projects <- list_projects(client = client)
subjects <- list_subjects("CENTRAL_OASIS", client = client)
experiments <- list_experiments("CENTRAL_OASIS", subject_id = "OAS1_0001", client = client)
scans <- list_scans("CENTRAL_OASIS", "OAS1_0001", "OAS1_0001_MR1", client = client)
```

For interactive work, you can also use the global session:

```r
authenticate_xnat(
  base_url = "https://central.xnat.org",
  username = "guest",
  password = "guest"
)

projects <- list_projects()
xnat_logout()
```

## Authentication

xnatR resolves credentials from these sources, in order:

1. explicit arguments to `xnat_connect()` or `authenticate_xnat()`
2. environment variables: `XNATR_HOST`, `XNATR_USER`, `XNATR_PASS`
3. `~/.xnatR_config.yml`, created with `initialize_config()`
4. `~/.netrc`, matched by XNAT host

```r
# Environment-variable workflow
Sys.setenv(
  XNATR_HOST = "https://central.xnat.org",
  XNATR_USER = "guest",
  XNATR_PASS = "guest"
)
client <- xnat_connect(use_jsession = TRUE)

# Create a config template, then edit ~/.xnatR_config.yml
initialize_config()
client <- xnat_connect()

# XNAT 1.8+ provider usernames use provider/username format
client <- xnat_connect(
  base_url = "https://myxnat.org",
  username = "ldap/myuser",
  password = "mypass"
)

# Reuse an existing JSESSIONID
client <- xnat_connect(
  base_url = "https://myxnat.org",
  jsession = Sys.getenv("XNAT_JSESSION"),
  verify = FALSE
)
```

Use `xnat_current_client()`, `xnat_server()`, `xnat_username()`,
`is_authenticated()`, and `xnat_logout()` to inspect or clear session state.

## Command Line

Install the command wrapper after installing the package:

```r
xnatR::install_cli("~/.local/bin", overwrite = TRUE)
```

If needed, add that directory to your `PATH`:

```sh
export PATH="$HOME/.local/bin:$PATH"
```

Check the command:

```sh
xnatR --help
```

The command reads credentials from `--host`, `--user`, and `--password`, or from
the same environment variables, config file, and `.netrc` sources as the R API.

```sh
export XNATR_HOST="https://central.xnat.org"
export XNATR_USER="guest"
export XNATR_PASS="guest"
```

List projects:

```sh
xnatR list-projects
```

List subjects as JSON:

```sh
xnatR list-subjects --project CENTRAL_OASIS --json
```

List sessions for a project or a subject:

```sh
xnatR list-experiments --project CENTRAL_OASIS
xnatR list-experiments --project CENTRAL_OASIS --subject OAS1_0001
```

List scans, resources, and files:

```sh
xnatR list-scans --project CENTRAL_OASIS --subject OAS1_0001 --experiment OAS1_0001_MR1
xnatR list-resources --project CENTRAL_OASIS --subject OAS1_0001 --experiment OAS1_0001_MR1 --scan 1
xnatR list-files --project CENTRAL_OASIS --subject OAS1_0001 --experiment OAS1_0001_MR1 --scan 1 --resource DICOM
```

Inspect DICOM header fields without downloading the images (DICOM-backed scans
only). Tags are comma-separated and accept `00080060` or `(0008,0060)` form;
omit `--fields` to dump everything:

```sh
xnatR dicom-dump --experiment OAS1_0001_MR1 --scan 1 --fields 00080060,00180080
```

Show the 10 most recent sessions:

```sh
xnatR recent-sessions --project CENTRAL_OASIS --n 10
```

Download an experiment archive:

```sh
xnatR download-experiment --experiment OAS1_0001_MR1 --dest-dir downloads --no-progress
```

You can also call the parser directly from R for advanced scripting:

```sh
Rscript -e 'xnatR::xnatR_cli(commandArgs(TRUE))' -- list-projects --json
```

## Listing and Browsing

All listing functions return tibbles with xnatR-specific print methods and
accept an optional `client` argument.

```r
# Projects, subjects, experiments, scans
list_projects(client = client)
list_subjects("CENTRAL_OASIS", limit = 50, client = client)
list_experiments("CENTRAL_OASIS", client = client)
list_experiments("CENTRAL_OASIS", subject_id = "OAS1_0001", client = client)
list_recent_sessions("CENTRAL_OASIS", n = 10, client = client)
list_scans("CENTRAL_OASIS", "OAS1_0001", "OAS1_0001_MR1", client = client)

# Scan-level resources and files
list_resources("CENTRAL_OASIS", "OAS1_0001", "OAS1_0001_MR1", "1", client = client)
list_files("CENTRAL_OASIS", "OAS1_0001", "OAS1_0001_MR1", "1", "DICOM", client = client)

# Experiment-level resources and files
list_experiment_resources("CENTRAL_OASIS", "OAS1_0001", "OAS1_0001_MR1", client = client)
list_experiment_files("CENTRAL_OASIS", "OAS1_0001", "OAS1_0001_MR1", "SNAPSHOTS", client = client)
list_experiment_files_all("OAS1_0001_MR1", client = client)

# API discovery
list_data_types(client = client)
list_queryable_fields("xnat:mrSessionData", client = client)
```

Interactive console browsing is available for projects and subjects:

```r
xnat_browse_projects(client = client)
xnat_browse_subjects("CENTRAL_OASIS", client = client)
```

In non-interactive contexts, pass `.interactive = FALSE` to return the listing
tibble without prompting.

### Fuzzy explorer (fzf)

If the [`fzf`](https://github.com/junegunn/fzf) command is on your `PATH`,
`xnat_explore()` opens a fuzzy-searchable, multi-select terminal browser that
drills down the project → subject → experiment → scan hierarchy with a preview
pane (DICOM headers at the scan level):

```r
chosen <- xnat_explore(client = client)

# Jump straight to a known session's scans:
xnat_explore("CENTRAL_OASIS", "OAS1_0001", "OAS1_0001_MR1", client = client)
```

Keys: `Enter` opens the highlighted item, `Tab` marks rows, `Ctrl-S` returns the
marked rows (e.g. to feed `download_experiment()`), `Ctrl-D` downloads them to
`dest_dir`, `Ctrl-E` jumps to every session in the project, `Ctrl-O` opens the
item in the web UI, and `Esc` goes back a level (or exits at the top).

## Downloading

```r
# Download an experiment by experiment ID
download_experiment(
  experiment_id = "OAS1_0001_MR1",
  dest_dir = "downloads",
  client = client
)

# Download all scans from an experiment using the project hierarchy
download_files(
  project_id = "CENTRAL_OASIS",
  subject_id = "OAS1_0001",
  experiment_id = "OAS1_0001_MR1",
  client = client
)

# Download selected scans or one resource
download_files(
  project_id = "CENTRAL_OASIS",
  subject_id = "OAS1_0001",
  experiment_id = "OAS1_0001_MR1",
  scan_id = c("1", "2"),
  resource = "DICOM",
  dest_dir = "downloads",
  client = client
)

# Download all experiments for a subject
download_subject("CENTRAL_OASIS", "OAS1_0001", dest_dir = "downloads", client = client)

# Download a known file path or URL
download_xnat_file(
  "/data/projects/CENTRAL_OASIS/subjects/OAS1_0001/experiments/OAS1_0001_MR1/scans/1/resources/DICOM/files/example.dcm",
  dest_file = "example.dcm",
  client = client
)
```

`download_experiment()` supports `format = "zip"` or `"tar.gz"`,
`extract = TRUE`, and `strict = FALSE` for workflows that should continue after
a failed download.

## Search

```r
# Search project metadata already returned by list_projects()
search_projects("OASIS", client = client)

# High-level scan search with named filters
search_scans(
  project_id = "CENTRAL_OASIS",
  scan_type = "T1",
  client = client
)

# XNAT XML search API
results <- xnat_search(
  root_type = "xnat:mrSessionData",
  fields = c("xnat:mrSessionData/ID", "xnat:mrSessionData/label"),
  criteria = list(
    list(
      field = "xnat:mrSessionData/project",
      comparison = "EQUALS",
      value = "CENTRAL_OASIS"
    )
  ),
  client = client
)

# Fluent search builder
results <- xnat_search_builder("xnat:mrSessionData", client = client) |>
  search_select("xnat:mrSessionData/ID", "xnat:mrSessionData/label") |>
  search_where("xnat:mrSessionData/project", "EQUALS", "CENTRAL_OASIS") |>
  search_execute()
```

## Alias Tokens

```r
token <- xnat_token_issue(client = client)

token_client <- xnat_connect(
  base_url = "https://central.xnat.org",
  username = token$alias,
  password = token$secret
)

xnat_token_validate(token$alias, token$secret, client = client)
xnat_token_list(client = client)
xnat_token_invalidate(token$alias, token$secret, client = client)
```

## Design

- `httr2` handles HTTP, authentication, retry, and downloads.
- Results are returned as `tibble` objects with lightweight S3 print methods.
- Functions accept explicit `xnat_client` objects and also support a global
  session for interactive use.
- The API is pipe-friendly and works with base R pipes and tidyverse workflows.

## License

MIT

## Transport selection and credential prompts

When R's embedded libcurl cannot connect, test both transports before entering a
password. Diagnostics send no credentials and always verify TLS:

```r
host <- "https://your-xnat.example/xnat"
xnat_diagnose(host, backend = "httr2")
xnat_diagnose(host, backend = "curl")

# Run in interactive Terminal R; install.packages("getPass") first.
client <- xnat_connect(host, backend = "curl",
                       credential_callback = xnat_credentials)
list_projects(client = client)
```

A reachable HTTP 401 means the transport works and authentication is required.
It does not mean login succeeded. A browser login does not authenticate this R
client. A callback can instead return `list(username = ..., password = ...)`, or
`NULL` to cancel (`xnatR_cancelled`). Do not embed passwords in scripts.

The callback is invoked only after a credential-free connectivity check. This
workflow establishes an in-memory JSESSION and discards the returned password.
On HTTP 401, a GET/HEAD request can call the callback once to renew the session
and retry once; HTTP 403 is a permission failure and never triggers renewal.
POST/DELETE requests are not replayed. Keep client objects in memory: serializing
them can persist session credentials or a callback's captured secrets.

`backend = "curl"` also works with supplied credentials, `use_jsession = TRUE`,
search requests, downloads, and `authenticate_xnat()`. System curl receives its
configuration and credentials through a pipe, never command arguments or a saved
configuration file; user `.curlrc` files are ignored. TLS verification defaults
to enabled. Redirects are reported explicitly rather than forwarding credentials.
System-curl requests currently have a 30-second connection timeout and a
300-second transfer timeout unless overridden (the saved-plan executor defaults to
3600 seconds), and do not automatically retry transient failures.
It requires the `processx` package and a system curl executable.

Failures have distinct classes: `xnatR_network_error`,
`xnatR_certificate_error`, `xnatR_authentication_error`, and
`xnatR_permission_error` (all inherit `xnatR_transport_error`).

Project-wide browser results can now open sessions with missing optional subject
fields. The public scan API also supports canonical experiment IDs alone:

```r
list_scans(experiment_id = "XNAT_E00001", client = client)
```

## Discover sessions and save an exact DICOM plan

`xnat_inventory()` searches all accessible projects when the project is unknown:

```r
inventory <- xnat_inventory(
  date_from = "2026-02-01", date_to = "2026-03-31",
  client = client
)
inventory$sessions[, c("project_id", "experiment_id", "subject_id", "date",
                        "participant_session_count", "scan_types")]
inventory$scans
```

Dates are inclusive. Missing or invalid session dates remain visible with
`date_status = "unknown"`; use `include_unknown_dates = FALSE` to exclude them.
Missing subject IDs are resolved by querying each canonical subject's sessions.
Unknown associations remain `NA`, and conflicting associations abort discovery.
Participant counts refer to retained sessions within each project, not lifetime
visits or cross-project identity. Labels are display metadata, never identifiers.

To narrow a later search, supply `project_ids` and/or `series_pattern = "audio|video"`.
The series expression matches scan type or description without case sensitivity;
it retains matching sessions **and all their scans**, with `series_match` marking
the matches. Names alone do not establish the scientific task: confirm that with
the owner before freezing a selection.

The inventory contains `projects`, `subjects`, `sessions`, `scans`, `resources`
and a `collections` traversal audit. Projects and subjects cover the requested
project scope; sessions, scans and scan resources reflect the date/series filters.
Experiment- and assessor-level resources are outside this workflow. The existing
`list_*()` functions retain their prior limit/offset behavior; use
`xnat_inventory()` for complete discovery.

Pagination continues until an empty page, even if the server caps the requested
page size. Overlaps, changing totals and exhausted page budgets are errors. The
`auto` mode handles legacy servers that repeat a page by requesting `limit=*`,
requiring `totalRecords` to match and previously observed rows to agree. Use
`pagination = "paged"` to forbid that fallback or `"all"` to request it explicitly.
This follows the documented [XNAT paging contract](https://wiki.xnat.org/xnat-api/paging-restful-tables).
Completeness refers to accessible collections at query time; XNAT does not provide
an atomic snapshot across these calls. Any request failure aborts the inventory.

After reviewing the candidates, save their exact canonical IDs:

```r
# Synthetic IDs: replace with the exact sessions you reviewed.
chosen <- c("XNAT_E00001", "XNAT_E00002")
plan <- xnat_plan_download(inventory, chosen,
                           dest_dir = "~/Downloads/dicom-archives",
                           client = client)
plan
plan$scans       # includes explicit no_dicom exclusions
plan$resources   # IDs, counts, sizes, fingerprints and archive destinations
plan$files       # full catalog: resource key, name, URI and exact byte size

xnat_write_plan(plan, "reviewed-selection.json")
plan <- xnat_read_plan("reviewed-selection.json")  # validates locally, offline
```

Planning rechecks project and subject membership, session/scan metadata and
resource identities. It reads complete catalogs for DICOM resources only, using
the documented [experiment-scoped resource endpoints](https://wiki.xnat.org/xnat-api/image-scan-resource-api).
Unknown identities, unknown file sizes, changed selections, and catalogs that
disagree with resource counts or sizes are refused. No DICOM bytes or destination
directories are created. By default all scans of selected sessions are reviewed;
pass an explicit `scans` data frame with `experiment_id` and `scan_id` columns to
select fewer. Every selected session must have a non-empty DICOM resource.

Plans preserve exact sizes as decimal strings, including values beyond R's
exact integer range. Destination components encode canonical IDs as UTF-8 hex,
so labels, path separators and case-insensitive filesystems cannot collapse
separate IDs into the same archive. File catalogs retain full server URIs;
repeated basenames are not silently deduplicated. `expected_bytes` describes
uncompressed catalog bytes, not ZIP transfer sizes or required free space.

The JSON is reviewable, contains no client object, and includes SHA-256 catalog
and plan fingerprints. The reader checks fingerprints, totals, relationships and
destinations. Fingerprints detect accidental edits; they are neither signatures
nor remote-file integrity checks. Writes default to refusing replacement and use
owner-only permissions on POSIX systems. Plans can contain sensitive research
identifiers and filenames; keep them in an appropriate local location.

## Execute and restart a reviewed plan

```r
plan <- xnat_read_plan("reviewed-selection.json")
result <- xnat_execute_plan(plan, client = client, download_timeout = 3600)
result$complete
result$resources[, c("experiment_id", "scan_id", "status", "error_class")]
result$summary_path

# After interruption or failure, run the same plan again.
result <- xnat_execute_plan(plan, client = client, download_timeout = 3600)
stopifnot(result$complete)
```

The executor uses the selected transport and requires `zip`, `filelock`, and a
system `unzip` executable. It downloads ZIPs without extracting them. Each attempt
gets a separate owner-only `.partial-*` file. Every ZIP file must match a
catalog-relative path and uncompressed size, with only a common XNAT wrapper
prefix permitted. Duplicate paths, traversal paths, non-regular members and
encrypted archives are rejected. System `unzip -tqq` must verify every CRC before
the archive is published. ZIP member sizes beyond the verifier's exact numeric
range (2^53 bytes) are refused.

Successful archives receive `.receipt.json` files binding the server, exact plan,
resource identity, catalog fingerprint, archive byte count and SHA-256 hash.
Reruns query the source catalogs again and reuse archives only after receipt,
hash, member and CRC checks pass. Invalid archives and stale receipts are renamed
with `.unverified-*` suffixes before retrying; failed partials are retained. Restart
is at resource/scan granularity, not byte-level resume. Existing archives without
matching receipts are not assumed verified.

Source membership, DICOM resource selection (including `no_dicom` exclusions) and
catalogs are checked before downloads; each transferred catalog is checked again
after verification. A final selection/catalog/receipt pass is required for
`complete = TRUE`. A changed source requires a newly reviewed plan, not a relaxed
comparison. Catalog checks detect metadata changes; same-name, same-size remote
content replacements are undetectable without remote content checksums. Local
SHA-256 hashes identify received bytes and do not prove remote authenticity.

A destination-wide advisory lock prevents concurrent xnatR executors. The lock
file remains in place; the operating system releases the lock when the process
exits, including a crash. Use a filesystem with functioning advisory locks.
Destination subdirectories and artifact files must not be symlinks. The lock
coordinates xnatR executors, not unrelated programs writing the same files.

The per-plan `run-summary-<hash>.json` is atomically updated. Each new run replaces
any earlier completion status with `state = "running", complete = FALSE` before
source checks. An interrupted or failed run never publishes whole-plan completion;
a force-killed process can leave a running summary, which is also incomplete.
The returned report includes every planned resource, verified/expected file
counts, and explicit exclusions. By default the executor stops after the first
resource failure; `continue_on_error = TRUE` attempts remaining resources after
ordinary failures. Source changes always stop the run, regardless of this option.
Configuration and lock failures throw before execution. Later errors or normal R
interrupts return an incomplete result with error classes and retained artifacts.
There are no automatic retries within an attempt; rerun the plan to retry.

The package now provides all three implementation tranches. Live qualification
still requires the actual reviewed eight-session job: discover through R, save
its exact selection, interrupt a transfer, restart without downloading verified
scans again, and account for every planned resource. Local synthetic server tests
cover these behaviors but do not establish acceptance on the private server.


The ordered live-acceptance steps, required evidence, and protection of the
existing downloader are recorded in the
[live qualification plan](.planning/LIVE-QUALIFICATION.md). Current progress,
validation evidence and the next action are tracked in that plan.

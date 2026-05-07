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

xnatR currently exposes an R API; it does not install a standalone `xnatR` shell
command or an `install_cli()` helper. For command-line use, call exported
functions with `Rscript -e` and pass credentials through environment variables,
a config file, or `.netrc`.

```sh
export XNATR_HOST="https://central.xnat.org"
export XNATR_USER="guest"
export XNATR_PASS="guest"

Rscript -e 'library(xnatR); client <- xnat_connect(); print(list_projects(client = client))'
```

List subjects:

```sh
Rscript -e 'library(xnatR); client <- xnat_connect(); print(list_subjects("CENTRAL_OASIS", client = client))'
```

List sessions for a project or a subject:

```sh
Rscript -e 'library(xnatR); client <- xnat_connect(); print(list_experiments("CENTRAL_OASIS", client = client))'
Rscript -e 'library(xnatR); client <- xnat_connect(); print(list_experiments("CENTRAL_OASIS", "OAS1_0001", client = client))'
```

Download an experiment archive:

```sh
Rscript -e 'library(xnatR); client <- xnat_connect(); download_experiment("OAS1_0001_MR1", dest_dir = "downloads", client = client)'
```

For longer workflows, put the R code in a script and run it with `Rscript`:

```r
#!/usr/bin/env Rscript
library(xnatR)

args <- commandArgs(trailingOnly = TRUE)
project_id <- args[[1]]

client <- xnat_connect()
print(list_recent_sessions(project_id, n = 10, client = client))
```

```sh
Rscript recent-sessions.R CENTRAL_OASIS
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

# xnatR

An R package for interacting with [XNAT](https://www.xnat.org/) neuroimaging archives via the REST API.

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("bbuchsbaum/xnatR")
```

## Quick Start

```r
library(xnatR)

# Option A: Global session (stored in the package environment)
authenticate_xnat(
  base_url = "https://central.xnat.org",
  username = "guest",
  password = "guest"
)

# List available projects
projects <- list_projects()

# List subjects in a project
subjects <- list_subjects(project_id = "MyProject")

# List experiments for a subject
experiments <- list_experiments(
  project_id = "MyProject",
  subject_id = "Subject001"
)

# List scans for an experiment
scans <- list_scans(
  project_id = "MyProject",
  subject_id = "Subject001",
  experiment_id = "Experiment001"
)
```

If you prefer not to use global session state, create an explicit client and pass
it to functions:

```r
client <- xnat_connect(
  base_url = "https://central.xnat.org",
  username = "guest",
  password = "guest",
  use_jsession = TRUE
)

projects <- list_projects(client = client)
```

## Authentication

xnatR supports multiple authentication methods (in priority order):

1. **Direct credentials**: Pass `username` and `password` to `authenticate_xnat()`
2. **Environment variables**: Set `XNATR_HOST`, `XNATR_USER`, `XNATR_PASS`
3. **Configuration file**: Create `~/.xnatR_config.yml` using `initialize_config()`
4. **.netrc file**: Add credentials to `~/.netrc`

```r
# Using environment variables
Sys.setenv(XNATR_HOST = "https://central.xnat.org")
Sys.setenv(XNATR_USER = "myuser")
Sys.setenv(XNATR_PASS = "mypass")
authenticate_xnat()

# Using JSESSION for better performance
authenticate_xnat(
  base_url = "https://central.xnat.org",
  username = "myuser",
  password = "mypass",
  use_jsession = TRUE
)

# XNAT 1.8+ with auth provider
authenticate_xnat(
  base_url = "https://myxnat.org",
  username = "ldap/myuser",  # provider/username format
  password = "mypass"
)
```

## Features

### Listing Functions

```r
# Projects, subjects, experiments, scans
list_projects()
list_subjects(project_id = "MyProject")
list_experiments(project_id = "MyProject", subject_id = "Subject001")
list_scans(project_id = "MyProject", subject_id = "Subject001", experiment_id = "Exp001")

# Resources and files
list_resources(project_id, subject_id, experiment_id, scan_id)
list_files(project_id, subject_id, experiment_id, scan_id, resource = "DICOM")

# Derived data
list_assessors(project_id, subject_id, experiment_id)
list_reconstructions(project_id, subject_id, experiment_id)

# API discovery
list_data_types()
list_queryable_fields("xnat:mrSessionData")
```

### Downloading

```r
# Download all scans from an experiment
download_files(
  project_id = "MyProject",
  subject_id = "Subject001",
  experiment_id = "Exp001"
)

# Download specific scans
download_files(
  project_id = "MyProject",
  subject_id = "Subject001",
  experiment_id = "Exp001",
  scan_id = c("1", "2", "3")
)

# Download specific resource
download_files(
  project_id = "MyProject",
  subject_id = "Subject001",
  experiment_id = "Exp001",
  resource = "DICOM"
)

# Download all data for a subject
download_subject(project_id = "MyProject", subject_id = "Subject001")
```

### Advanced Search

```r
# Search using the XML search API
results <- xnat_search(
  root_type = "xnat:mrSessionData",
  fields = c("xnat:mrSessionData/ID", "xnat:mrSessionData/label"),
  criteria = list(
    list(field = "xnat:mrSessionData/project", comparison = "EQUALS", value = "MyProject")
  )
)

# Fluent search builder
results <- xnat_search_builder("xnat:mrSessionData") |>
  search_select("xnat:mrSessionData/ID", "xnat:mrSessionData/label") |>
  search_where("xnat:mrSessionData/project", "EQUALS", "MyProject") |>
  search_where("xnat:mrSessionData/date", "GREATER_THAN", "2023-01-01") |>
  search_execute()

# Simple project search
search_projects("MRI")
```

### Alias Tokens

```r
# Issue a new token
token <- xnat_token_issue()

# List active tokens
xnat_token_list()

# Validate a token
xnat_token_validate(alias = token$alias, secret = token$secret)

# Invalidate a token
xnat_token_invalidate(alias = token$alias, secret = token$secret)
```

## Session Management

```r
# Check authentication status
is_authenticated()

# Get current server
xnat_server()

# Get current username
xnat_username()

# Logout
xnat_logout()

# Logout and invalidate server session
xnat_logout(invalidate_session = TRUE)
```

## Design

xnatR is built on modern R patterns:

- **httr2** for HTTP handling with automatic retry and error handling
- **tibble** for all return values with S3 classes and custom print methods
- **Functional style** - pure functions with explicit state management
- **Pipe-friendly** - works seamlessly with `|>` and tidyverse

## License

MIT

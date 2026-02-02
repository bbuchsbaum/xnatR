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

# Authenticate with XNAT server
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

## Authentication

xnatR supports multiple authentication methods:

1. **Direct credentials**: Pass `username` and `password` to `authenticate_xnat()`
2. **Environment variables**: Set `XNATR_HOST`, `XNATR_USER`, `XNATR_PASS`
3. **Configuration file**: Create `~/.xnatR_config.yml` using `initialize_config()`

## Features

- List projects, subjects, experiments, and scans
- Download files and archives from XNAT
- Search projects by substring
- Session-based authentication with automatic retry
- Built on httr2 for modern HTTP handling

## License

MIT

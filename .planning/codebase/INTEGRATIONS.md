# External Integrations

**Analysis Date:** 2026-01-26

## APIs & External Services

**XNAT (Extensible Neuroimaging Archive Toolkit):**
- Primary external system integration
- REST API interface for neuroimaging data management
- SDK/Client: Custom implementation using httr R package
- Auth: Basic Authentication (username:password or username:token)
  - Environment variable: Not used; credentials from `~/.xnatR_config.yml`
  - Auth mechanism: Base64-encoded Basic Auth header via `base64enc::base64encode()`
  - See: `R/authenticate.R` lines 38-47

**XNAT API Endpoints:**
- `GET /data/projects` - List all projects
- `GET /data/projects/{projectId}/subjects` - List subjects in project
- `GET /data/projects/{projectId}/subjects/{subjectId}/experiments` - List experiments for subject
- `GET /data/projects/{projectId}/subjects/{subjectId}/experiments/{experimentId}/scans` - List scans for experiment
- `GET /data/projects/{projectId}/subjects/{subjectId}/experiments/{experimentId}/scans/{scanId}/resources/{resource}/files?format=zip` - Download specific resource files
- `GET /data/projects/{projectId}/subjects/{subjectId}/experiments/{experimentId}/scans/{scanId}/files?format=zip` - Download all scan files

All API calls constructed in respective R files:
- Projects: `R/list_projects.R` line 22
- Subjects: `R/list_subjects.R` lines 29-33
- Experiments: `R/list_experiments.R` lines 33-38
- Scans: `R/list_scans.R` lines 37-43
- File downloads: `R/download_files.R` lines 75-84

## Data Storage

**Databases:**
- None - xnatR is a client library only
- All data stored on remote XNAT server instance

**File Storage:**
- Local filesystem - Downloaded XNAT data saved to user-specified directories via `httr::write_disk()`
- Default download location: Current working directory (configurable via `dest_dir` parameter)
- File naming pattern: `{project_id}_{subject_id}_{experiment_id}_{scan_id}_{resource}.zip`
- See: `R/download_files.R` lines 88-94

**Caching:**
- In-memory environment-based caching of authentication credentials
- Implementation: Package-level environment `xnatR_env` in `R/environment.R`
- Cached items:
  - `auth_header` - Base64-encoded authentication header
  - `base_url` - XNAT server base URL
  - `ssl_verify` - SSL verification flag
- Cache populated by: `R/authenticate.R` lines 76-78
- Cache checked before all API calls (e.g., `R/list_projects.R` lines 17-18)

## Authentication & Identity

**Auth Provider:**
- Custom implementation wrapping XNAT's Basic Authentication
- Two authentication methods supported:
  1. Username + Password (classic HTTP Basic Auth)
  2. Username + API Token (token acts as password in Basic Auth)

**Implementation:**
- Entry point: `authenticate_xnat()` in `R/authenticate.R`
- Credentials sourced from:
  - Explicit parameters: `base_url`, `username`, `password`, `token`, `ssl_verify`
  - YAML config file: `~/.xnatR_config.yml` (created by `initialize_config()`)
- Flow:
  1. Load credentials from config via `load_credentials()` if not explicitly provided
  2. Construct auth string: `username:password` or `username:token`
  3. Base64 encode via `base64enc::base64encode(charToRaw(auth_string))`
  4. Add Authorization header: `Basic {encoded_string}` to all subsequent requests
  5. Test authentication via GET to `/data/projects` endpoint
  6. Cache auth_header, base_url, ssl_verify in package environment `xnatR_env`

**Credentials File Format:**
- Location: `~/.xnatR_config.yml`
- Format: YAML with keys: `base_url`, `username`, `password` (or `token`), `ssl_verify`
- Template created by: `R/config.R` lines 17-32
- Loaded by: `R/config.R` lines 46-65, uses `yaml::yaml.load_file()`

**SSL/TLS:**
- Configurable via `ssl_verify` parameter (defaults to TRUE)
- When disabled: `httr::config(ssl_verifypeer = FALSE)`
- Used in all API calls throughout package

## Monitoring & Observability

**Error Tracking:**
- None - No external error tracking service integrated
- Errors handled via R's `stop()` function with descriptive messages

**Logs:**
- Console output via `message()` and `warning()` functions
- Logging locations:
  - Authentication: "Authentication successful/failed" in `R/authenticate.R`
  - File operations: "File downloaded successfully to {dest_file}" in `R/download.R`, `R/download_files.R`
  - Directory creation: "Created destination directory: {dest_dir}" in `R/download_files.R`, `R/download_subject.R`, `R/download_all_subjects.R`
  - Batch operations: Progress messages for subjects/experiments in `R/download_subject.R` and `R/download_all_subjects.R`
- HTTP response debugging: Raw response content printed on error via `httr::content(response, as = "text")`

## CI/CD & Deployment

**Hosting:**
- CRAN (Comprehensive R Archive Network) - Target distribution platform
- GitHub repository - Development/version control (inferred from git structure)

**CI Pipeline:**
- None detected - No CI configuration files (e.g., .github/workflows/, .travis.yml, appveyor.yml)

## Environment Configuration

**Required env vars:**
- None - Package uses YAML config file exclusively for credentials
- All configuration via `~/.xnatR_config.yml` or function parameters

**Secrets location:**
- `~/.xnatR_config.yml` - User's home directory, plaintext YAML storage
- Security note: Credentials stored in plaintext file; relies on OS file permissions for access control
- No environment variable redirection or encrypted storage implemented

## Webhooks & Callbacks

**Incoming:**
- None - xnatR is a client library; does not expose endpoints

**Outgoing:**
- None - No webhook callbacks to external services
- Only HTTP GET requests to XNAT server

---

*Integration audit: 2026-01-26*

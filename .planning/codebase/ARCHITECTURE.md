# Architecture

**Analysis Date:** 2026-01-26

## Pattern Overview

**Overall:** Client Library Pattern with Layered Facade

**Key Characteristics:**
- RESTful API client library wrapping XNAT HTTP endpoints
- Stateful authentication caching using package environment
- Functional decomposition by XNAT resource hierarchy (Projects → Subjects → Experiments → Scans)
- Hierarchical data retrieval matching XNAT's nested resource structure
- Bulk operation support via iterative traversal of hierarchy

## Layers

**Authentication Layer:**
- Purpose: Manage credentials, authenticate with XNAT server, maintain session state
- Location: `R/authenticate.R`, `R/config.R`, `R/environment.R`
- Contains: Credential loading, Basic Auth header generation, SSL configuration
- Depends on: `httr`, `base64enc`, user's YAML config file
- Used by: All API functions require authenticated state in `xnatR_env`

**API Client Layer:**
- Purpose: Execute HTTP requests to XNAT REST endpoints with consistent authentication
- Location: All functions in `R/list_*.R`, `R/download*.R`, `R/search_projects.R`
- Contains: URL construction, HTTP GET requests, JSON response parsing
- Depends on: `httr`, `jsonlite`, authentication state from `xnatR_env`
- Used by: Application-facing query and download functions

**Configuration Layer:**
- Purpose: Manage user credentials and system configuration
- Location: `R/config.R`, `R/environment.R`
- Contains: Config file initialization, credential loading
- Depends on: YAML config file at `~/.xnatR_config.yml`
- Used by: `authenticate_xnat()` and optional parameter resolution

**Resource Hierarchy Traversal:**
- Purpose: Model XNAT's nested resource structure for batch operations
- Location: `R/download_files.R` (download_subject, download_all_subjects functions)
- Contains: Iterative traversal of projects → subjects → experiments
- Depends on: List functions for discovering nested resources
- Used by: Bulk download operations

## Data Flow

**Authentication Flow:**

1. User calls `initialize_config()` to create template configuration file at `~/.xnatR_config.yml`
2. User manually edits config file with XNAT credentials (base_url, username, password or token)
3. User calls `authenticate_xnat()` with optional parameter overrides
4. `authenticate_xnat()` loads credentials via `load_credentials()` using YAML parsing
5. Creates Basic Auth header using `base64enc::base64encode()`
6. Performs test HTTP GET to `/data/projects` endpoint to verify authentication
7. Caches auth header and base URL in `xnatR_env` private package environment
8. Returns invisibly with success message

**Query Data Flow (e.g., list_subjects):**

1. User calls `list_subjects(project_id = "TEST")`
2. Function validates project_id parameter
3. Checks `xnatR_env$auth_header` is present (authentication guard)
4. Constructs URL: `{base_url}/data/projects/{project_id}/subjects`
5. URL-encodes project_id parameter
6. Retrieves SSL configuration from `xnatR_env$ssl_verify`
7. Executes `httr::GET()` with Authorization header
8. Parses JSON response using `jsonlite::fromJSON()`
9. Extracts `ResultSet$Result` from parsed structure
10. Converts to R data frame and returns

**Download Data Flow (single file):**

1. User calls `download_xnat_file(url, dest_file)`
2. Checks authentication state in `xnatR_env`
3. Executes `httr::GET()` with `httr::write_disk()` to stream directly to file
4. Returns invisibly if status code 200, stops with error otherwise

**Bulk Download Flow (entire subject):**

1. User calls `download_all_subjects(project_id, dest_dir)`
2. Calls `list_subjects()` to discover all subjects
3. For each subject, calls `download_subject(project_id, subject_id, dest_dir)`
4. Inside download_subject: calls `list_experiments()` to discover experiments
5. For each experiment, calls `download_files()` to download all scans
6. Returns list of responses invisibly

**State Management:**

- Authentication state persists in `xnatR_env` (private package environment)
- State includes: `auth_header` (Base64 encoded credentials), `base_url` (XNAT server URL), `ssl_verify` (boolean)
- Credentials are cached in memory for entire R session
- No state cleanup on logout (user must restart R session to clear)
- Each API function validates state before execution, stopping if not authenticated

## Key Abstractions

**XNAT Resource Hierarchy:**
- Purpose: Model XNAT's nested resource structure (Project contains Subjects, Subjects contain Experiments, etc.)
- Examples:
  - `list_projects()` - top level
  - `list_subjects(project_id)` - one level deep
  - `list_experiments(project_id, subject_id)` - two levels deep
  - `list_scans(project_id, subject_id, experiment_id)` - three levels deep
- Pattern: Each level takes parent IDs as parameters, constructs endpoint URL hierarchically

**HTTP Request Wrapper:**
- Purpose: Centralize httr request handling with authentication and SSL configuration
- Implementation: Each function repeats similar pattern:
  1. Validate parameters
  2. Check `xnatR_env$auth_header` exists
  3. Construct SSL config from `xnatR_env$ssl_verify`
  4. Execute httr::GET with consistent headers
  5. Parse JSON response
  6. Handle errors with descriptive messages

**Credential Management:**
- Purpose: Support both password and token-based authentication, store in config file
- Examples:
  - File location: `~/.xnatR_config.yml`
  - Formats: YAML with `base_url`, `username`, `password` or `token`, `ssl_verify`

**Batch Operations:**
- Purpose: Enable downloading entire data hierarchies without manual iteration
- Examples: `download_all_subjects()`, `download_subject()`
- Pattern: Discover resources at current level, iterate and call child operations recursively/sequentially

## Entry Points

**Configuration Setup:**
- Location: `R/config.R`
- Functions: `initialize_config()`, `load_credentials()`
- Triggers: First-time setup, manual credential refresh
- Responsibilities: Create/load YAML config file, provide credentials to authentication layer

**Authentication:**
- Location: `R/authenticate.R`
- Function: `authenticate_xnat()`
- Triggers: User calls function explicitly or via first API call if auto-credentials available
- Responsibilities: Load credentials, test connection, cache auth state in `xnatR_env`

**Project Listing (Top-Level Query):**
- Location: `R/list_projects.R`
- Function: `list_projects()`
- Triggers: User wants to discover available projects
- Responsibilities: Execute GET to /data/projects, parse results, return data frame

**Hierarchical Queries:**
- Location: `R/list_subjects.R`, `R/list_experiments.R`, `R/list_scans.R`, `R/search_projects.R`
- Functions: `list_subjects()`, `list_experiments()`, `list_scans()`, `search_projects()`
- Triggers: User needs to navigate XNAT hierarchy or search for specific projects
- Responsibilities: Construct endpoint URLs, execute queries, return filtered/parsed results

**File Downloads:**
- Location: `R/download.R`, `R/download_files.R`
- Functions: `download_xnat_file()`, `download_files()`, `download_subject()`, `download_all_subjects()`
- Triggers: User wants to retrieve actual data files from XNAT server
- Responsibilities: Build download URLs, stream to disk, manage directory creation, report progress

## Error Handling

**Strategy:** Fail-fast validation with explicit stopping

**Patterns:**
- Parameter validation: `if (missing(project_id) || !is.character(project_id)) stop(...)`
- Authentication guards: `if (is.null(xnatR_env$auth_header)) stop("Not authenticated")`
- HTTP status checks: `if (httr::status_code(response) != 200) stop(...)`
- Response format validation: `if (!is.null(content_parsed$ResultSet$Result))` before proceeding
- Detailed error output: Print raw HTTP response for debugging
- Warnings for non-fatal issues: `warning("No projects found...")` when returning empty results
- Invisible returns on success: All functions return `invisible()` to suppress output

## Cross-Cutting Concerns

**Logging:**
- Uses `message()` for informational output (connection success, downloads completed, directories created)
- Uses `warning()` for non-critical issues (empty results, SSL warnings)
- Uses `stop()` for fatal errors with descriptive context
- HTTP response bodies printed via `print(httr::content(...))` for debugging authentication/API errors

**Validation:**
- Input validation: All functions validate required parameters (character type, non-missing)
- URL encoding: All user-supplied path parameters encoded via `utils::URLencode(project_id, reserved = TRUE)`
- Authentication validation: All API functions check `xnatR_env$auth_header` and `xnatR_env$base_url` exist
- Response structure validation: Check for expected JSON structure before parsing

**Authentication:**
- Credentials loaded from `~/.xnatR_config.yml` (YAML format)
- Cached in `xnatR_env` private environment during session
- Supports Basic Auth via Base64-encoded credentials header
- Supports token-based auth (API token treated as password equivalent in Basic Auth scheme)
- SSL verification configurable via config file and parameter overrides

---

*Architecture analysis: 2026-01-26*

# Testing Patterns

**Analysis Date:** 2026-01-26

## Test Framework

**Runner:**
- Not detected - No testthat, RUnit, or other R testing framework configured
- No `tests/` or `testthat/` directory present
- No test configuration files found

**Assertion Library:**
- Not detected - No testing library integrated

**Run Commands:**
```bash
# Testing infrastructure not configured
# Use standard R package check
R CMD check xnatR

# Manual testing in interactive R session
source("R/authenticate.R")
authenticate_xnat()
projects <- list_projects()
```

## Test File Organization

**Location:**
- No test files detected in the codebase
- No standard R test directories: `tests/`, `testthat/`, `inst/tests/`

**Naming:**
- Not applicable - testing not implemented

**Structure:**
- Not applicable - testing not implemented

## Test Structure

**Suite Organization:**
- Not implemented
- No test suites, test cases, or test runners configured

**Patterns:**
- Manual testing only (implied from roxygen examples)
- Functions include `@examples` sections with `\dontrun{}` wrapped examples
- Example from `R/authenticate.R`:
  ```r
  #' @examples
  #' \dontrun{
  #' download_xnat_files(
  #'   url = "https://rrinid.rotman-baycrest.on.ca/spred/data/projects/BuBr_M190_BA/subjects/BuBr_M190_BA_14972/experiments/BuBr_M190_BA_14972_MRI_20240108/scans/ALL/resources/DICOM/files?format=zip",
  #'   username = "buchsbaum",
  #'   password = "password",
  #'   dest_file = "BuBr_M190_BA_MRI_14972__20240108_Scan_ALL.zip"
  #' )
  #' }
  ```

## Mocking

**Framework:**
- Not detected - No mocking library configured (mockito, unittest.mock, or R equivalent)

**Patterns:**
- Not implemented - Functions directly call external APIs via `httr` package
- No fixtures or test doubles defined

**What to Mock (if testing is added):**
- HTTP responses from XNAT API (`httr::GET()`)
- File system operations (`dir.exists()`, `dir.create()`, `file.path()`)
- Credential loading from configuration files
- Authentication headers and SSL configuration

**What NOT to Mock (if testing is added):**
- Data frame manipulation with `dplyr::filter()`
- JSON parsing with `jsonlite::fromJSON()`
- URL encoding with `utils::URLencode()`
- String operations and message formatting

## Fixtures and Factories

**Test Data:**
- Not defined - No test data or fixtures present

**Location:**
- No `tests/fixtures/`, `tests/data/`, or similar directory
- No factory functions for creating test objects

**How to Implement (if testing is added):**
- Create mock XNAT API responses for common scenarios
- Example for project listing:
  ```r
  mock_projects_response <- list(
    ResultSet = list(
      Result = data.frame(
        ID = c("TEST1", "TEST2"),
        name = c("Test Project 1", "Test Project 2")
      )
    )
  )
  ```

## Coverage

**Requirements:**
- No coverage requirements enforced
- Not configured in package configuration

**View Coverage:**
- Not applicable - testing infrastructure not in place
- Would require testthat + covr packages:
  ```bash
  # If implemented
  devtools::test_coverage()
  ```

## Test Types

**Unit Tests:**
- Not implemented
- Would test:
  - Parameter validation functions (character checks, missing parameter checks)
  - Configuration loading functions
  - URL encoding and API endpoint construction
  - JSON response parsing and data frame conversion

**Integration Tests:**
- Not implemented
- Would test:
  - Full authentication workflow
  - Retrieve operations (list_projects, list_subjects, etc.)
  - Download operations with file I/O
  - Error handling with actual API responses

**E2E Tests:**
- Not implemented
- Would require:
  - Access to live XNAT server (blocked by `\dontrun{}` in examples)
  - Test credentials and projects
  - Network connectivity
  - Would test complete workflows: authenticate → search → download

**Manual Testing Approach (Current):**
- Examples in roxygen documentation are wrapped in `\dontrun{}`
- Developers must manually test in interactive R session
- Requires active XNAT server connection
- Examples from `R/list_projects.R`:
  ```r
  #' \dontrun{
  #'   authenticate_xnat()
  #'   projects <- list_projects()
  #'   print(projects)
  #' }
  ```

## Common Patterns

**Async Testing:**
- Not applicable - R functions are synchronous, `httr` calls are blocking

**Error Testing:**
- Not implemented
- Would test:
  - Missing required parameters: `missing(project_id)`
  - Invalid parameter types: `!is.character(project_id)`
  - Failed authentication: `is.null(xnatR_env$auth_header)`
  - HTTP error responses: `httr::status_code(response) != 200`
  - Missing configuration: File not found for `load_credentials()`

**Validation Pattern (for reference):**
```r
# From R/download_files.R - pattern used in all functions
if (missing(project_id) || !is.character(project_id)) {
  stop("Please provide a valid 'project_id' as a character string.", call. = FALSE)
}
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
  message("Created destination directory: ", dest_dir)
}
```

## Testing Recommendations

**High Priority (would catch real bugs):**
1. Unit tests for parameter validation - all functions have identical validation patterns
2. Unit tests for URL construction - potential for encoding errors in `R/download_files.R`
3. Integration tests for API response parsing - JSON structure varies by endpoint
4. Authentication flow tests - central to all operations

**Medium Priority:**
1. Error handling tests - verifies graceful failure modes
2. File operations tests - directory creation, file writing
3. Configuration loading tests - handles missing/invalid YAML

**Low Priority:**
1. Data frame filtering tests - `search_projects()` uses `dplyr::filter()`
2. Message/warning output tests - verifies user feedback

**Testing Framework Recommendation:**
- Use `testthat` package (standard for R)
- Add to DESCRIPTION: `Suggests: testthat (>= 3.0.0)`
- Create `tests/testthat/` directory
- Implement fixtures for mocking HTTP responses using `httptest2` or `webmockr`

---

*Testing analysis: 2026-01-26*

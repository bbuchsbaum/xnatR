# Codebase Concerns

**Analysis Date:** 2026-01-26

## Tech Debt

**Duplicated Download Functions:**
- Issue: Two separate download functions exist with nearly identical functionality: `download_xnat_file()` and `download_xnat_files()` in `R/authenticate.R`
- Files: `R/authenticate.R` (lines 85-131), `R/download.R` (lines 19-56)
- Impact: Code duplication increases maintenance burden and increases risk of inconsistent behavior between similar operations
- Fix approach: Consolidate into single download function with consistent parameters. Choose one implementation as canonical and deprecate the other.

**Incomplete Documentation in authenticate.R:**
- Issue: Example code in `download_xnat_files()` is malformed/incomplete (line 101 shows "pa" on its own line)
- Files: `R/authenticate.R` (lines 99-103)
- Impact: Users copying documentation examples will encounter syntax errors; reduces usability of exported function
- Fix approach: Correct the example to show complete, working code snippet

**Module-Level File Functions:**
- Issue: `download_files.R` contains three separate exported functions (`download_files()`, `download_subject()`, `download_all_subjects()`) mixed with comment headers suggesting they should be separate files (lines 124, 203)
- Files: `R/download_files.R`
- Impact: Makes file navigation confusing; violates R package convention of one exported function per file
- Fix approach: Split into three files: `R/download_files.R`, `R/download_subject.R`, `R/download_all_subjects.R`

## Security Considerations

**Credentials Stored in Plain Text:**
- Risk: Configuration file `.xnatR_config.yml` stores passwords and API tokens in plain YAML without encryption
- Files: `R/config.R` (lines 21-28), `R/authenticate.R` (lines 16-32)
- Current mitigation: Users are responsible for file permissions on home directory
- Recommendations:
  - Use R keyring package to store credentials in OS credential storage
  - Warn users explicitly about plaintext storage in documentation
  - Consider environment variables as alternative authentication method

**SSL Verification Disabled in Production:**
- Risk: Code allows disabling SSL certificate verification (ssl_verify=FALSE), which exposes credentials to MITM attacks
- Files: `R/authenticate.R` (lines 54-58), `R/download.R` (lines 31-35), `R/download_files.R` (lines 97-101), `R/list_projects.R` (lines 25-29), and 5+ other files
- Current mitigation: Defaults to ssl_verify=TRUE, but documented as configurable
- Recommendations:
  - Remove ability to disable SSL verification entirely OR
  - Require explicit environment variable consent (e.g., XNATR_INSECURE_SSL)
  - Add security warning when ssl_verify=FALSE is configured

**Basic Auth Transmission:**
- Risk: Base64 encoding in `authenticate_xnat()` is not encryption; credentials transmitted in Authorization header over HTTP if ssl_verify=FALSE is used
- Files: `R/authenticate.R` (lines 39-47)
- Current mitigation: SSL/TLS via httr at transport layer
- Recommendations:
  - Document that HTTPS is required for all connections
  - Consider API token-only authentication (password support adds risk)
  - Add validation that base_url must be HTTPS

**Credentials Printed in Error Messages:**
- Risk: When authentication fails, response content is printed to console which may include sensitive error details
- Files: `R/authenticate.R` (line 71), `R/download.R` (line 53), `R/list_projects.R` (line 41), `R/list_subjects.R` (line 52), `R/list_experiments.R` (line 57), `R/list_scans.R` (line 62), `R/download_files.R` (line 119)
- Current mitigation: None
- Recommendations:
  - Use `warning()` instead of `print()` for error responses
  - Truncate response content to avoid leaking sensitive information
  - Log full responses only in verbose mode

## Performance Bottlenecks

**No Retry Logic on Network Failures:**
- Problem: Single network call with no retry mechanism; transient network errors cause immediate failure
- Files: All API call functions in `R/list_*.R` and `R/download*.R`
- Cause: Direct httr::GET() calls without error handling or exponential backoff
- Improvement path:
  - Create wrapper function around httr::GET() with configurable retries
  - Use exponential backoff for retries
  - Document retry behavior to users

**Inefficient Bulk Downloads:**
- Problem: `download_all_subjects()` loops through subjects sequentially, each making separate HTTP calls
- Files: `R/download_files.R` (lines 224-268)
- Cause: Loop-based approach (for loop at line 248) means all downloads are serial
- Improvement path:
  - Consider parallel downloads using `future` package or `parallel`
  - Implement connection pooling if XNAT server supports concurrent requests
  - Add progress tracking that shows multiple downloads simultaneously

**Data Frame Coercion Overhead:**
- Problem: Every list function (list_projects, list_subjects, etc.) coerces raw JSON to data frame with `as.data.frame()`
- Files: `R/list_projects.R` (line 51), `R/list_subjects.R` (line 62), `R/list_experiments.R` (line 67), `R/list_scans.R` (line 72)
- Cause: Type coercion from nested JSON to flat data frame can be slow for large result sets
- Improvement path:
  - Return raw parsed JSON list by default with option to convert to data frame
  - Use `data.table::as.data.table()` for better performance on large datasets
  - Cache responses to avoid re-querying same data

## Fragile Areas

**Hardcoded JSON Response Path:**
- Files: `R/list_projects.R` (line 50), `R/list_subjects.R` (line 61), `R/list_experiments.R` (line 66), `R/list_scans.R` (line 71), `R/download_files.R` (throughout)
- Why fragile: All functions assume response has structure `ResultSet$Result`. If XNAT API changes response format or returns different structure, all list functions silently fail or error
- Safe modification: Extract response parsing into separate utility function with test coverage. Use defensive checks for path existence.
- Test coverage: No test files exist to validate response parsing. No tests for malformed responses.

**Environment Variable Coupling:**
- Files: `R/environment.R` (lines 4-9), used by all API functions
- Why fragile: Package state stored in `xnatR_env` global environment. Multiple authentication calls overwrite state. No session isolation.
- Safe modification: Create session objects (R6 classes) instead of global environment to allow multiple concurrent authenticated sessions
- Test coverage: No tests for authentication state management or concurrent session handling

**URL Encoding Inconsistency:**
- Files: `R/list_subjects.R` (line 32), `R/list_experiments.R` (line 36-37), `R/list_scans.R` (line 40-42), `R/download_files.R` (line 66-70)
- Why fragile: Some functions use `utils::URLencode(..., reserved = TRUE)` while others use simpler approaches
- Safe modification: Create utility function for consistent URL encoding. Use `reserved = TRUE` consistently for project/subject/experiment IDs
- Test coverage: No tests for URL encoding of special characters in IDs

**API Endpoint Hardcoding:**
- Files: All API call files hardcode paths like `/data/projects`, `/data/subjects`, `/data/experiments`
- Why fragile: No API versioning strategy. Future XNAT versions with different endpoints would break all functions
- Safe modification: Extract API endpoints to configuration constants at package level. Allow user-configurable API base path.

## Known Bugs

**Incomplete Example in authenticate.R:**
- Symptoms: Documentation example cannot be run as-is due to syntax error
- Files: `R/authenticate.R` (line 101 shows "pa" on incomplete line)
- Trigger: Run the example code from ?download_xnat_files
- Workaround: Manually complete the password parameter

**Missing Required Dependency:**
- Symptoms: `yaml` package used in config.R but not listed in DESCRIPTION
- Files: `R/config.R` (line 52 uses `yaml::yaml.load_file()`)
- Trigger: Call `load_credentials()` without yaml installed
- Workaround: Manually install yaml package

## Missing Critical Features

**No Connection Pooling or Session Management:**
- Problem: Each function creates new HTTP connection, no connection reuse
- Blocks: Efficient batch operations; high latency for sequential queries

**No Rate Limiting Handling:**
- Problem: No detection of or adaptation to XNAT rate limits
- Blocks: Batch downloading with `download_all_subjects()` may trigger rate limiting without retry logic

**No Caching Layer:**
- Problem: Repeated calls to `list_projects()`, `list_subjects()` etc. re-query server each time
- Blocks: Efficient exploratory workflows

**No Async/Non-blocking Downloads:**
- Problem: Large file downloads block R session
- Blocks: Interactive usage during downloads

## Test Coverage Gaps

**No Automated Tests:**
- What's not tested: Every exported function lacks unit or integration tests
- Files: All R/ files have zero test coverage
- Risk: Changes to authentication flow, response parsing, or URL construction could break silently
- Priority: **High** - Authentication and data retrieval are critical paths

**API Response Variation Not Tested:**
- What's not tested: Handling of edge cases (empty results, malformed JSON, partial responses, rate limiting)
- Files: `R/list_*.R` functions assume well-formed responses
- Risk: Unexpected API responses cause crashes instead of graceful failures
- Priority: **High** - Used in production against external systems

**Authentication State Management Not Tested:**
- What's not tested: Multiple authentication attempts, credential reloading, session expiration
- Files: `R/authenticate.R`, `R/environment.R`
- Risk: Subtle state bugs in production use
- Priority: **Medium** - Edge case but critical when it fails

**Error Handling Not Tested:**
- What's not tested: Network errors, invalid credentials, missing files, invalid directory paths
- Files: Error handling in lines like 40-43 (list_projects.R), 56-59 (list_subjects.R)
- Risk: Users get unclear error messages instead of actionable guidance
- Priority: **Medium** - Impacts user experience

---

*Concerns audit: 2026-01-26*

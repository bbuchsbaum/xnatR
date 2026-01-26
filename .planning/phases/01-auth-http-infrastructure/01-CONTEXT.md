# Phase 1: Auth & HTTP Infrastructure - Context

**Gathered:** 2026-01-26
**Status:** Ready for planning

<domain>
## Phase Boundary

Complete authentication and HTTP foundation. Researchers can authenticate using any credential source (env vars, .netrc, YAML config, explicit params) with automatic session management, retry logic, and centralized HTTP helpers. Builds on existing `authenticate_xnat()` and `xnatR_env` patterns rather than rewriting from scratch.

</domain>

<decisions>
## Implementation Decisions

### Credential priority & discovery
- Priority order (highest to lowest): explicit function params > environment variables > .netrc > YAML config (~/.xnatR_config.yml)
- Environment variables: `XNAT_HOST`, `XNAT_USER`, `XNAT_PASS`, `XNAT_TOKEN` (standard naming, consistent prefix)
- .netrc support: use standard `~/.netrc` file, match by hostname from base_url — this is what curl and httr already understand
- Keep existing YAML config at `~/.xnatR_config.yml` as lowest-priority fallback
- No warning on conflicts — silently use highest-priority source. Users expect layered credential resolution (like git config)
- `authenticate_xnat()` gains a `verbose` parameter; when TRUE, prints which credential source was used

### Session lifecycle & transparency
- Maintain existing `xnatR_env` package environment pattern — it works, R users expect it
- Add JSESSIONID-based session auth: after initial Basic Auth, cache the session cookie and use it for subsequent requests (reduces credential exposure)
- Auto-refresh on 401: when any request gets HTTP 401, silently re-authenticate using cached credentials and retry the original request once
- No user-visible messages on silent re-auth unless `verbose = TRUE`
- Add `xnat_logout()` function that invalidates the JSESSIONID and clears `xnatR_env`
- No session timeout tracking on the client side — let the server decide, handle 401 when it comes
- Support connecting to multiple XNAT servers: `authenticate_xnat()` returns a connection object that can be passed to functions, while also setting the default in `xnatR_env` for convenience

### Error & retry behavior
- Use `stop()` with informative condition objects (classed errors: `xnat_auth_error`, `xnat_http_error`, `xnat_timeout_error`) so users can tryCatch specific error types
- HTTP 401: attempt silent re-auth once, then error with "Authentication failed — check credentials"
- HTTP 403: error with "Permission denied — you may not have access to this resource"
- HTTP 404: error with "Resource not found" plus the URL path (not full URL to avoid credential leakage)
- HTTP 5xx: retry up to 3 times with exponential backoff (1s, 2s, 4s), then error with "Server error — the XNAT server may be temporarily unavailable"
- Network/timeout errors: retry up to 3 times with same backoff, then error
- No retries on 4xx errors (except 401 re-auth) — these are deterministic failures
- Default request timeout: 60 seconds, configurable via option `xnatR.timeout`
- Never print raw HTML error pages to console — parse meaningful info or use status code message

### Configuration file conventions
- Keep existing YAML config location: `~/.xnatR_config.yml`
- Keep `initialize_config()` for creating template — it already works
- Config file is the lowest-priority credential source (convenience for interactive use)
- No project-local config files — keep it simple, env vars handle per-project needs
- Add `yaml` to DESCRIPTION Imports (currently used but not declared)
- R options for runtime tuning: `xnatR.timeout`, `xnatR.retries`, `xnatR.verbose` — function params override these

### Claude's Discretion
- Internal HTTP helper function design (single function vs. verb-specific wrappers)
- Exact exponential backoff implementation details
- Whether to use httr or httr2 (httr2 is more modern but httr is already a dependency)
- Alias token management function signatures (generate, validate, invalidate)
- How to structure classed error conditions internally

</decisions>

<specifics>
## Specific Ideas

- Existing `xnatR_env` pattern works well for single-server use — preserve it as the default, add connection objects for multi-server
- Follow the pattern of packages like `gh` and `googledrive` where auth "just works" with sensible defaults
- XNAT uses JSESSIONID cookies for session management — switching from Basic Auth header on every request to session cookie after initial auth is both more efficient and more secure
- The `download_xnat_files()` function has syntax errors and appears unmaintained — cleanup is fair game in this phase since it touches auth/HTTP

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 01-auth-http-infrastructure*
*Context gathered: 2026-01-26*

# xnatR

## What This Is

A comprehensive R client package for the XNAT REST API, enabling neuroimaging researchers to authenticate, query, search, and download data from any XNAT server instance. Initially targeting institute-level distribution with potential CRAN release. The package provides a functional API (not object-oriented) that covers the full read surface of the XNAT REST API.

## Core Value

Researchers can programmatically access and download any data from an XNAT server using idiomatic R functions, with robust authentication and session management that "just works."

## Requirements

### Validated

- ✓ Basic authentication with username/password — existing
- ✓ Token-based authentication (API token as password in Basic Auth) — existing
- ✓ List projects from authenticated server — existing
- ✓ List subjects within a project — existing
- ✓ List experiments for a subject — existing
- ✓ List scans for an experiment — existing
- ✓ Search projects by substring (client-side) — existing
- ✓ Download single file from XNAT — existing
- ✓ Download files for a specific scan/experiment — existing
- ✓ Download all data for a subject — existing
- ✓ Download all subjects in a project — existing
- ✓ YAML-based credential configuration — existing
- ✓ SSL/TLS verification toggle — existing

### Active

**Authentication & Session Management:**
- [ ] JSESSION-based session creation (POST /data/JSESSION)
- [ ] Session cookie reuse across API calls (avoid re-authenticating)
- [ ] Session invalidation / logout (DELETE /data/JSESSION)
- [ ] Automatic session refresh on 401 responses
- [ ] Alias token API (issue, validate, invalidate tokens)
- [ ] Auth provider prefix support (XNAT 1.8+ `provider/username` syntax)
- [ ] User Auth Service API (PUT /data/services/auth) as fallback
- [ ] Environment variable credential support (XNATR_HOST, XNATR_USER, XNATR_PASS)
- [ ] .netrc credential file support
- [ ] Credential source priority: explicit params > env vars > .netrc > YAML config

**HTTP Infrastructure:**
- [ ] Internal HTTP request helper (centralized auth, headers, SSL, error handling)
- [ ] Retry logic with exponential backoff for transient network errors
- [ ] Configurable response format (JSON, XML, CSV) via `format` parameter
- [ ] Proper HTTP status code handling (401 → refresh, 404 → clear error, 500 → server error)
- [ ] Support for all HTTP verbs (GET, PUT, POST, DELETE) in the internal layer

**Query & Listing Enhancements:**
- [ ] List resources for a scan (files, DICOM, etc.)
- [ ] List assessors for an experiment
- [ ] List reconstructions for an experiment
- [ ] List project users and permissions
- [ ] List available data types on server (GET /data/search/elements)
- [ ] List queryable fields for a data type (GET /data/search/elements/{xsiType})
- [ ] Flexible query parameters on list functions (columns, sortBy, limit, offset)

**Search API:**
- [ ] Server-side search via POST /data/search with XML search documents
- [ ] Search builder helpers (construct XML from R parameters)
- [ ] Discover searchable data types and fields
- [ ] Return search results as data frames
- [ ] Support for stored/saved search templates

**Download Enhancements:**
- [ ] Download multiple scans by comma-separated IDs
- [ ] Download scans by type (T1, T2, etc.) instead of numeric ID
- [ ] Download using `ALL` keyword for all scans in a session
- [ ] Download assessor files
- [ ] Download reconstruction files
- [ ] Progress reporting for large downloads
- [ ] Parallel/async download support for batch operations

**Package Quality:**
- [ ] testthat test suite with httptest2 mock HTTP responses
- [ ] roxygen2 documentation for all exported functions
- [ ] Package vignette with usage examples
- [ ] Remove unused DESCRIPTION imports (ggplot2, shiny, shinydashboard, plotly)
- [ ] Add yaml to DESCRIPTION Imports (currently missing)
- [ ] Consolidate duplicate download functions (authenticate.R vs download.R)
- [ ] Fix malformed documentation example in authenticate.R
- [ ] R CMD check passes with no warnings or notes

### Out of Scope

- Write operations (create subjects, upload DICOM, modify experiments) — deferred to future milestone
- Shiny dashboard UI — not part of API client scope
- Pipeline management endpoints — specialized, defer
- Admin/system configuration endpoints (/xapi/siteConfig) — admin use case, defer
- DICOM anonymization — handled by XNAT server
- Prearchive management — write operation, defer

## Context

**XNAT Platform:**
- Open-source neuroimaging archive toolkit from Washington University
- REST API uses hierarchical paths: projects → subjects → experiments → scans → resources → files
- Two URL prefix families: legacy `/data` (interchangeable with `/REST`, `/data/archive`) and newer `/xapi` (Swagger-enabled, not interchangeable)
- Responses available as JSON, XML, CSV, HTML, or ZIP
- Sessions expire after configurable period; clients must handle 401 refresh

**Existing Codebase (843 lines R across 10 files):**
- Functional style with global package environment for auth state
- Each list function repeats auth check + URL build + HTTP GET + JSON parse pattern
- Two duplicate download functions exist (authenticate.R and download.R)
- No tests, no CI, yaml dependency not declared in DESCRIPTION
- Unused imports: ggplot2, shiny, shinydashboard, plotly

**Reference Implementation:**
- Python clients xnatpy and pyxnat provide similar functionality
- xnatpy supports: env vars, .netrc, session management, full CRUD
- This package aims for comparable read-side coverage in R

## Constraints

- **Language**: R — this is an R package, all implementation in R
- **API style**: Functional — keep current `authenticate_xnat()` / `list_projects()` style, no R6/S4 classes
- **HTTP client**: httr — already established, no migration to httr2 unless necessary
- **Testing**: httptest2 or webmockr — must test without live XNAT server for CRAN
- **Compatibility**: XNAT 1.7.6+ — support legacy /data endpoints; gracefully handle 1.8+ features
- **CRAN**: Package must pass R CMD check with no warnings for eventual CRAN submission

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Functional API (not R6/S4) | User preference; simpler API, consistent with current codebase | — Pending |
| Read-first, write later | Focus on comprehensive read coverage before adding CRUD | — Pending |
| httptest2 for mock testing | CRAN-compatible testing without live XNAT server | — Pending |
| Keep httr (not httr2) | Avoid rewrite of existing working code | — Pending |
| Support XNAT 1.7.6+ | Broadest compatibility; degrade gracefully for 1.8+ features | — Pending |

---
*Last updated: 2026-01-26 after initialization*

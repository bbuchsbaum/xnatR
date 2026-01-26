# Requirements: xnatR

**Defined:** 2026-01-26
**Core Value:** Researchers can programmatically access and download any data from an XNAT server using idiomatic R functions, with robust authentication and session management that "just works."

## v1 Requirements

Requirements for initial release. Each maps to roadmap phases.

### Authentication & Sessions

- [ ] **AUTH-01**: User can create a JSESSION via POST /data/JSESSION with Basic credentials
- [ ] **AUTH-02**: Session cookie is automatically reused across subsequent API calls
- [ ] **AUTH-03**: User can invalidate session via DELETE /data/JSESSION (logout)
- [ ] **AUTH-04**: Automatic session refresh when server returns 401 (re-auth and retry)
- [ ] **AUTH-05**: User can issue alias tokens via GET /data/services/tokens/issue
- [ ] **AUTH-06**: User can validate alias tokens via GET /data/services/tokens/validate/{token}/{secret}
- [ ] **AUTH-07**: User can invalidate alias tokens via GET /data/services/tokens/invalidate/{token}/{secret}
- [ ] **AUTH-08**: Auth provider prefix support (provider/username syntax for XNAT 1.8+)
- [ ] **AUTH-09**: Credentials readable from environment variables (XNATR_HOST, XNATR_USER, XNATR_PASS)
- [ ] **AUTH-10**: Credentials readable from .netrc file
- [ ] **AUTH-11**: Credential priority: explicit params > env vars > .netrc > YAML config

### HTTP Infrastructure

- [ ] **HTTP-01**: Internal request helper centralizing auth headers, SSL config, and error handling
- [ ] **HTTP-02**: Retry with exponential backoff on transient network failures (configurable max retries)
- [ ] **HTTP-03**: Configurable response format (JSON, XML, CSV) via format parameter on all query functions
- [ ] **HTTP-04**: Internal support for GET, PUT, POST, DELETE HTTP verbs (foundation for future write ops)

### Query & Listing

- [ ] **QURY-01**: User can list resources and files for a scan
- [ ] **QURY-02**: User can list assessors for an experiment
- [ ] **QURY-03**: User can list reconstructions for an experiment
- [ ] **QURY-04**: User can list all available data types on server (GET /data/search/elements)
- [ ] **QURY-05**: User can list queryable fields for a data type (GET /data/search/elements/{xsiType})
- [ ] **QURY-06**: All list functions support flexible query parameters (columns, sortBy, limit, offset)

### Search API

- [ ] **SRCH-01**: User can submit XML search via POST /data/search and receive results as data frame
- [ ] **SRCH-02**: R helper functions construct search XML from R parameters (no hand-written XML required)
- [ ] **SRCH-03**: User can discover searchable data types and their queryable fields programmatically
- [ ] **SRCH-04**: User can save, load, and reuse search templates

### Download Enhancements

- [ ] **DWNL-01**: User can download multiple scans by comma-separated IDs in a single request
- [ ] **DWNL-02**: User can download scans by type name (T1, T2, BOLD) instead of numeric ID
- [ ] **DWNL-03**: User can download all scans in a session using the ALL keyword
- [ ] **DWNL-04**: User can download files from assessors
- [ ] **DWNL-05**: User can download files from reconstructions
- [ ] **DWNL-06**: Download functions display progress for large file transfers
- [ ] **DWNL-07**: Batch download operations can run in parallel for improved throughput

### Package Quality

- [ ] **QUAL-01**: testthat test suite with httptest2 mock HTTP responses covering all exported functions
- [ ] **QUAL-02**: DESCRIPTION cleaned: unused imports removed (ggplot2, shiny, shinydashboard, plotly); yaml added to Imports
- [ ] **QUAL-03**: Duplicate download functions consolidated; malformed documentation examples fixed
- [ ] **QUAL-04**: Package vignette demonstrating authentication, listing, searching, and downloading workflows
- [ ] **QUAL-05**: R CMD check passes with no warnings or notes

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### Write Operations

- **WRITE-01**: User can create a subject in a project (PUT /data/archive/projects/{P}/subjects/{S})
- **WRITE-02**: User can update subject demographics via query parameters
- **WRITE-03**: User can create an experiment/session under a subject
- **WRITE-04**: User can delete subjects, experiments, or scans
- **WRITE-05**: User can upload DICOM files via POST /data/services/import

### Administration

- **ADMIN-01**: User can list and manage project users/permissions
- **ADMIN-02**: User can query site configuration (/xapi/siteConfig)
- **ADMIN-03**: User can manage prearchive entries

## Out of Scope

| Feature | Reason |
|---------|--------|
| Shiny dashboard UI | Not part of API client package; separate project if needed |
| Pipeline management | Specialized admin feature, low priority for client users |
| DICOM anonymization | Handled server-side by XNAT |
| Real-time event subscriptions | Complex, rare use case |
| R6/S4 class-based API | User preference for functional style |
| httr2 migration | Would require full rewrite of working code |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| AUTH-01 | — | Pending |
| AUTH-02 | — | Pending |
| AUTH-03 | — | Pending |
| AUTH-04 | — | Pending |
| AUTH-05 | — | Pending |
| AUTH-06 | — | Pending |
| AUTH-07 | — | Pending |
| AUTH-08 | — | Pending |
| AUTH-09 | — | Pending |
| AUTH-10 | — | Pending |
| AUTH-11 | — | Pending |
| HTTP-01 | — | Pending |
| HTTP-02 | — | Pending |
| HTTP-03 | — | Pending |
| HTTP-04 | — | Pending |
| QURY-01 | — | Pending |
| QURY-02 | — | Pending |
| QURY-03 | — | Pending |
| QURY-04 | — | Pending |
| QURY-05 | — | Pending |
| QURY-06 | — | Pending |
| SRCH-01 | — | Pending |
| SRCH-02 | — | Pending |
| SRCH-03 | — | Pending |
| SRCH-04 | — | Pending |
| DWNL-01 | — | Pending |
| DWNL-02 | — | Pending |
| DWNL-03 | — | Pending |
| DWNL-04 | — | Pending |
| DWNL-05 | — | Pending |
| DWNL-06 | — | Pending |
| DWNL-07 | — | Pending |
| QUAL-01 | — | Pending |
| QUAL-02 | — | Pending |
| QUAL-03 | — | Pending |
| QUAL-04 | — | Pending |
| QUAL-05 | — | Pending |

**Coverage:**
- v1 requirements: 37 total
- Mapped to phases: 0
- Unmapped: 37 ⚠️

---
*Requirements defined: 2026-01-26*
*Last updated: 2026-01-26 after initial definition*

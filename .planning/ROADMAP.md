# Roadmap: xnatR

## Overview

Building on existing basic auth and listing functions, this roadmap completes the comprehensive R client for XNAT REST API. Four phases deliver robust authentication with multiple credential sources, complete query and discovery capabilities, powerful XML search functionality, and enhanced download operations with package quality improvements for CRAN release.

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [ ] **Phase 1: Auth & HTTP Infrastructure** - Complete authentication and HTTP foundation
- [ ] **Phase 2: Query & Discovery** - Complete listing and discovery capabilities
- [ ] **Phase 3: Search API** - Build XML search functionality
- [ ] **Phase 4: Download & Quality** - Enhanced downloads and package cleanup

## Phase Details

### Phase 1: Auth & HTTP Infrastructure
**Goal**: Researchers can authenticate using any credential source (env vars, .netrc, YAML, explicit params) with automatic session management and retry logic.

**Depends on**: Nothing (builds on existing auth)

**Requirements**: AUTH-01, AUTH-02, AUTH-03, AUTH-04, AUTH-05, AUTH-06, AUTH-07, AUTH-08, AUTH-09, AUTH-10, AUTH-11, HTTP-01, HTTP-02, HTTP-03, HTTP-04

**Success Criteria** (what must be TRUE):
  1. User can authenticate with credentials from environment variables without writing YAML config
  2. User can authenticate using .netrc file without explicit credentials
  3. Session automatically refreshes when server returns 401 and retries the original request
  4. User can generate, validate, and invalidate alias tokens programmatically
  5. All HTTP requests use centralized helper with consistent auth, SSL, retry, and error handling

**Plans**: TBD

Plans:
- [ ] 01-01: TBD
- [ ] 01-02: TBD

### Phase 2: Query & Discovery
**Goal**: Researchers can list any XNAT resource and discover available data types and queryable fields programmatically.

**Depends on**: Phase 1

**Requirements**: QURY-01, QURY-02, QURY-03, QURY-04, QURY-05, QURY-06

**Success Criteria** (what must be TRUE):
  1. User can list resources and files for any scan
  2. User can list assessors and reconstructions for experiments
  3. User can discover what data types exist on their XNAT server
  4. User can query which fields are available for any data type
  5. All list functions accept flexible query parameters (columns, sortBy, limit, offset)

**Plans**: TBD

Plans:
- [ ] 02-01: TBD

### Phase 3: Search API
**Goal**: Researchers can perform complex XML searches without writing XML by hand, using R helper functions that construct search queries.

**Depends on**: Phase 2

**Requirements**: SRCH-01, SRCH-02, SRCH-03, SRCH-04

**Success Criteria** (what must be TRUE):
  1. User can execute XML search queries and receive results as data frames
  2. User can construct searches using R functions without hand-writing XML
  3. User can discover searchable types and fields programmatically
  4. User can save and reuse search templates for common queries

**Plans**: TBD

Plans:
- [ ] 03-01: TBD
- [ ] 03-02: TBD

### Phase 4: Download & Quality
**Goal**: Package passes R CMD check with robust download capabilities including batch operations, type-based selection, and progress tracking.

**Depends on**: Phase 3

**Requirements**: DWNL-01, DWNL-02, DWNL-03, DWNL-04, DWNL-05, DWNL-06, DWNL-07, QUAL-01, QUAL-02, QUAL-03, QUAL-04, QUAL-05

**Success Criteria** (what must be TRUE):
  1. User can download multiple scans by ID, by type name (T1, T2), or using ALL keyword
  2. User can download files from assessors and reconstructions
  3. Download operations display progress and can run in parallel
  4. Package has comprehensive test suite with mocked HTTP responses
  5. R CMD check passes with no warnings or notes; package ready for CRAN submission

**Plans**: TBD

Plans:
- [ ] 04-01: TBD
- [ ] 04-02: TBD

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 3 → 4

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Auth & HTTP Infrastructure | 0/TBD | Not started | - |
| 2. Query & Discovery | 0/TBD | Not started | - |
| 3. Search API | 0/TBD | Not started | - |
| 4. Download & Quality | 0/TBD | Not started | - |

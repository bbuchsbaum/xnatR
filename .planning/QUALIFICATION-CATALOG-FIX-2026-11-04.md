# File-catalog protocol correction and qualification receipt

Recorded: 2026-11-04 (local date). Live planning remains open.

## Observed refusal and protocol basis

The user's plan-only retry failed at 2026-11-05T00:27:03Z at a scan-resource
files endpoint. The recorded reason was: `Cannot verify complete limit=*
response: totalRecords is missing or mismatched.` The response body was not
retained, so that record alone does not distinguish an absent total from a
contradictory one.

The collector applied a paged-table completeness rule to an endpoint documented
as a listing of all files for one resource. XNAT's official response example has
`title = "Files"` and no `totalRecords`; its documented parameters do not include
offset pagination. Sources:
[Image Scan Resource API](https://wiki.xnat.org/xnat-api/image-scan-resource-api),
[Paging RESTful tables](https://wiki.xnat.org/xnat-api/paging-restful-tables).

The correction is limited to auto mode, key URI, the exact experiment/scan/resource
files collection route, and no caller-supplied filters. It requests `limit=*`
once, requires a recognized Files response when a total is absent, rejects
malformed result envelopes and duplicate URIs, and checks any supplied total
against the returned row count. A contradictory total still fails. Other tables,
filtered requests, and explicit paged/all modes retain the prior strict behavior.

Completeness here relies on the documented all-files API contract, rather than an
invented total count. Available resource counts/bytes, source rechecks and later
ZIP-member/CRC verification remain consistency checks. This does not establish
remote content authenticity or qualify a server that violates that API contract.

## Frozen source and validation

- Commit: `96fc10517b0dfb70bbaade852f2ba4f1d7de28f2`.
- Package SHA-256:
  `438cb57c8ee1b107e964f3b5386c024a95a7cb6bc23da077ea8e3cf7b1edd9ac`.
- Workspace: `/private/tmp/xnatr-live-qualification-20261104/catalog-fix`.
- Package: `build/xnatR_0.2.0.tar.gz`; isolated installation: `library/xnatR`.
- Installed-package tests: **795 passed, one missing-fzf skip, zero failures,
  errors or warnings**. Both transport backends' planning/restart fixtures now
  serve file listings without totalRecords.
- `R CMD check --no-manual --no-tests`: **Status: OK**; installed tests ran
  separately and both vignettes built.
- The new regression reproduced the failure on the previous implementation.
  Tests cover missing totals, contradictory/invalid totals, malformed envelopes,
  duplicate URIs, empty catalogs and preservation of other pagination rules.
- Launcher checks: 15 resume/privacy checks, 23 scope/privacy checks and nine
  source-transition checks passed. Syntax and activated-library checks passed.

Evidence is under `evidence/` in this workspace: source/artifact hashes,
environment, before/targeted/installed-test logs, build/install/check logs,
inventory-compatibility.json and launcher checks. The previous build at `6e80224`
and all prior failure records remain available.

## Retained inventory and next action

The live eight-session inventory was captured under `6e80224`; its receipt is
unchanged. A separately verified compatibility record binds that inventory hash
and selection to `96fc105`: the inventory function body/formals are identical,
all 274 recorded collection routes are outside the changed file-catalog branch,
and shared parsing/request/authentication implementations are unchanged.
This supports reusing discovery evidence, not transferring live planning or
execution acceptance between revisions.

The existing launcher now loads the new isolated package and prints source
`96fc105`. Resume with:

```sh
/private/tmp/xnatr-live-qualification-20261104/discover.command --plan-only
```

The planner rechecks live membership, metadata, resources and file catalogs. A
future plan receipt will separately record its current package source and the
inventory's original source. No live plan or xnatR archive transfer has completed.
The all-project discovery HTTP 403 remains a separate unresolved gate.

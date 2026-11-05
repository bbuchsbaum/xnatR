# Live qualification: eight-session discovery-to-verified-download workflow

Recorded and resumed: 2026-11-04. Status: **source frozen and locally validated;
confirmed-project discovery and live plan review complete; archive execution pending**.

All three implementation tranches are locally delivered. The frozen implementation
at `96fc10517b0dfb70bbaade852f2ba4f1d7de28f2` has 795 passing installed-package
assertions, one real-fzf test skipped, and a package check with Status: OK (tests
run separately). These are local results, not acceptance on the private server.
See [the qualification receipt](QUALIFICATION-2026-11-04.md) for the artifact hash,
environment, regression fix, evidence locations and credential-entry launcher.
The [catalog-fix receipt](QUALIFICATION-CATALOG-FIX-2026-11-04.md) records the
current build, endpoint-specific correction and retained inventory provenance.

The original handoff recorded the agreed sequence. The resumed work completed
step 1 and authentication in step 2. All-project discovery stopped with HTTP 403;
the failed endpoint was not captured. The next attempt is explicitly scoped to
the owner-confirmed project, retaining strict completeness inside that scope.
That scoped attempt subsequently completed discovery and matched all eight
expected session/subject pairs. After the file-catalog correction, live planning
and saved-plan review passed. This does not qualify the original all-project
discovery component. No live download has started.

## Scope and protection of existing work

Use the existing owner-confirmed eight-session selection as the comparison
reference. Discover the candidates through R and verify canonical IDs and
membership; labels or generic audio/video series names are not independent proof
of task identity. Do not invent missing IDs or broaden the selection.

Keep the existing downloader, archives, partials, receipts and summaries untouched.
Use a separate qualification destination with adequate capacity. Do not stop,
modify or relaunch the existing downloader, share its output paths, or assume its
receipts satisfy xnatR's receipt contract. Coordinate the full transfer so it does
not disrupt an ongoing download. No extraction or BIDS conversion is in scope.

Keep exact participant/session IDs, catalogs, request evidence and credentials out
of repository documents and shared memory. Store private qualification artifacts
in an appropriate local location. Shared records contain status and evidence
references only, never passwords, tokens, cookies or participant payloads.

## Ordered steps and evidence

- [x] **1. Freeze the implementation and isolate the installation.** Review the
  current changes, create a local commit, and record its SHA. Build/install that
  exact source into a dedicated R library. Record the source artifact hash, R and
  dependency versions, system curl/unzip versions and validation results. Do not
  replace the default R library or publish a release at this stage.

- [ ] **2. Establish the live R discovery route.** Run credential-free diagnostics
  before asking for credentials, with TLS verification enabled. Use the explicit
  system-curl backend and hidden Terminal prompt or supplied credential callback.
  Browser login does not authenticate the R client. Run date-filtered
  `xnat_inventory()` across accessible projects, inspect participant/series
  summaries, and compare the resulting canonical session identities and subject
  associations with the existing owner-confirmed selection. Retain unknowns and
  failures explicitly; do not guess or weaken completeness checks.

- [x] **3. Save and review the exact eight-session plan.** Use
  `xnat_plan_download()` and `xnat_write_plan()` to freeze the selection in the
  separate qualification destination. Inspect all eight canonical session IDs,
  subject membership, selected scans, DICOM resource IDs, complete file catalogs,
  expected counts, exact uncompressed sizes, destination paths and `no_dicom`
  exclusions. Read the saved JSON back with `xnat_read_plan()`. Record the plan
  fingerprint and private artifact location. Do not treat uncompressed byte totals
  as a prediction of ZIP transfer size or required free space.

- [ ] **4. Demonstrate interruption and verified restart.** Start executing the
  frozen plan. Allow at least one resource to finish verification and receive its
  receipt; interrupt a subsequent resource while it is transferring. Preserve the
  partial attempt and incomplete report. Restart the **same full eight-session
  plan**, from the same saved JSON and destination: receipts are bound to the
  exact plan fingerprint, so a separate pilot/subset plan is not interchangeable.
  Require the completed resource to report `reused`, the interrupted resource to
  be downloaded and verified, and the prior failed attempt to remain retained.
  Keep evidence that the reused resource had no archive-download GET on restart
  (metadata requests are expected), alongside its unchanged archive hash and
  receipt. Do not enable credential-bearing verbose request logs to obtain this
  evidence. A running/interrupted summary must never count as complete.

- [ ] **5. Establish whole-selection completeness.** Continue the same saved plan
  to completion. Require `complete = TRUE`, successful final source checks, and
  equality of verified/expected resource and file counts. Account for every
  planned DICOM resource and every explicit scan exclusion; there must be no
  pending, failed, interrupted or source-changed planned resources. Retain the
  final summary, per-resource receipts, catalog fingerprints and archive hashes.
  Re-run the unchanged plan and require every planned resource to be reused with
  no archive downloads. Completion is a whole-selection result, not an inference
  from a few successful receipts or the historical downloader's progress count.

- [ ] **6. Close qualification and prepare delivery.** Record the exact tested
  commit, saved-plan fingerprint, actual denominators, restart evidence, final
  completeness and any limitations. If qualification requires a code fix, commit
  it and repeat the affected checks against the new frozen source; do not transfer
  acceptance silently between revisions. Then prepare the release or PR, with
  only synthetic examples and non-sensitive validation evidence in its payload.

## Refusal and acceptance rules

A changed source catalog, membership or DICOM selection stops qualification for
that plan. Resolve the change and create a newly reviewed plan explicitly; do not
relax comparisons, rewrite a receipt as verified, or retry a changed source until
it appears to pass. Network, certificate, authentication and permission failures
remain distinct from archive-integrity failures.

Local SHA-256 hashes identify received bytes. Catalog rechecks cannot detect a
same-name, same-size remote content replacement without remote checksums. ZIP
CRC validation establishes archive integrity, not independent remote authenticity.
The acceptance statement must retain these limits.

**Exit criterion:** the actual reviewed eight-session selection is discovered
through R, saved reproducibly, interrupted and restarted without re-fetching
verified resources, and fully accounted for under the frozen plan and tested
source revision. Until that evidence exists, live qualification remains open.

## Current evidence and next action

- Source freeze and isolated installation: complete for the corrected `96fc105`
  build. Installed-package tests: 795 passed, one fzf skip, no failures, errors or
  warnings. `R CMD check --no-manual --no-tests`: Status: OK; both vignettes built
  successfully.
- Credential-free diagnostics on the original `6e80224` build: system curl reaches the
  server with TLS verified and HTTP 401; httr2 reports a network failure. This
  establishes reachability only, not authenticated access or a diagnosed root cause.
- Existing downloader: its current private summary reports completion for eight
  sessions and 241 scans. The summary was read without modifying the downloader
  or its artifacts; this is not xnatR qualification or an independent archive audit.
- Authentication succeeded on the next user run; subsequent all-project discovery
  failed with HTTP 403. The failing request is unknown because that launcher saved
  only an error class. Project-scoped continuation and private endpoint diagnostics
  were prepared and synthetically tested. The next scoped run completed discovery
  for eight sessions, 241 scans and 241 DICOM resources; all eight canonical
  session/subject pairs match the private reference. Its saved inventory and hash
  receipt are retained locally.
- Live planning then failed with `xnatR_inventory_error`. The original diagnostic
  omitted the validation reason. Offline replay with historical catalogs passed,
  which neither explains the live failure nor qualifies those catalogs as current.
- The next live retry identified the exact refusal: missing/mismatched totalRecords
  at a scan-resource files endpoint. The corrected build recognizes the documented
  all-files response when a total is absent, while rejecting supplied contradictory
  totals, duplicate URIs and malformed responses. Other collection pagination
  retains its strict rules. That revision subsequently completed live planning
  at 2026-11-05T00:49:19Z. The saved JSON passed package validation and independent
  identity, selection, receipt, destination and integer-total checks.
- Reviewed plan: eight sessions, 241 scans/resources, 13,715 files,
  28,731,975,360 uncompressed bytes and zero excluded scans. Fingerprint:
  `795b525309b28868849a1cda41b3a6a7f3da2719b7cad7f0a3ce55c2364d2ee7`.
  Evidence: `private/discovery-receipt.json`, `private/reviewed-plan.json` and
  `private/plan-review.json` in the qualification workspace.
- Interruption/reuse evidence, live transfer counts and final xnatR summary:
  **not yet established**. No qualification archive directory exists yet.
- Next action: prepare execution of this exact saved plan, then follow steps 4-5.
  All-project discovery remains an open gate. No credentials belong in chat.

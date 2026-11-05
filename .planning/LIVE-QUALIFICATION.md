# Live qualification: eight-session discovery-to-verified-download workflow

Recorded and resumed: 2026-11-04. Status: **source frozen and locally validated;
authentication succeeded; discovery incomplete after HTTP 403**.

All three implementation tranches are locally delivered. The frozen implementation
at `6e80224a001027fad4fcbacbd311bd5c4a9452d4` has 775 passing installed-package
assertions, one real-fzf test skipped, and a package check with Status: OK (tests
run separately). These are local results, not acceptance on the private server.
See [the qualification receipt](QUALIFICATION-2026-11-04.md) for the artifact hash,
environment, regression fix, evidence locations and credential-entry launcher.

The original handoff recorded the agreed sequence. The resumed work completed
step 1 and authentication in step 2. All-project discovery stopped with HTTP 403;
the failed endpoint was not captured. The next attempt is explicitly scoped to
the owner-confirmed project, retaining strict completeness inside that scope.
This does not qualify the original all-project discovery component. No live
download has started.

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

- [ ] **3. Save and review the exact eight-session plan.** Use
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

- Source freeze and isolated installation: complete. Installed-package tests:
  775 passed, one fzf skip, no failures, errors or warnings. `R CMD check
  --no-manual --no-tests`: Status: OK; both vignettes built successfully.
- Credential-free diagnostics on that installed build: system curl reaches the
  server with TLS verified and HTTP 401; httr2 reports a network failure. This
  establishes reachability only, not authenticated access or a diagnosed root cause.
- Existing downloader: its current private summary reports completion for eight
  sessions and 241 scans. The summary was read without modifying the downloader
  or its artifacts; this is not xnatR qualification or an independent archive audit.
- Authentication succeeded on the next user run; subsequent all-project discovery
  failed with HTTP 403. The failing request is unknown because that launcher saved
  only an error class. Project-scoped continuation and private endpoint diagnostics
  are prepared and synthetically tested; live success remains unverified.
- Live plan fingerprint, complete authenticated discovery, interruption/reuse evidence,
  live transfer counts and final xnatR summary: **not yet established**.
- Next action: enter credentials locally using the prepared Terminal launcher
  documented in the receipt. It now explicitly scopes discovery to the confirmed
  project and saves the separate plan; all-project discovery remains an open gate.
  archive execution follows review under steps 3-5. No credentials belong in chat.

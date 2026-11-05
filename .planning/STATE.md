# Project State

Updated: 2026-11-04.

## Current focus

The three discovery-to-verified-download implementation tranches are frozen at
`96fc10517b0dfb70bbaade852f2ba4f1d7de28f2` and installed in a new isolated library.
Installed-package validation: 795 passing assertions, one real-fzf skip, no test
failures/errors/warnings; package check Status: OK with tests run separately.
Source changes now stop execution even with `continue_on_error = TRUE`.
The previous `6e80224` build and evidence remain preserved. See the
[catalog-fix receipt](QUALIFICATION-CATALOG-FIX-2026-11-04.md).

Step 1 of [LIVE-QUALIFICATION.md](LIVE-QUALIFICATION.md) is complete. Step 2 has
passed credential-free system-curl connectivity with TLS enabled and HTTP 401;
httr2 still reports a network failure. User-entered credentials now successfully
authenticate R, but all-project discovery stopped with HTTP 403. Its original
failure record lacks an endpoint, so the denied request remains unidentified.
The subsequent confirmed-project run completed authenticated discovery: eight
sessions, 241 scans, and 241 DICOM resources. Independent comparison of the saved
inventory matched all eight expected session/subject pairs. The subsequent
planning retry identified a missing/mismatched `totalRecords` refusal at a scan
resource file catalog. The corrected collector uses the documented all-files
response contract in auto mode and still rejects conflicting reported totals.
The next gate is **successful live planning with the corrected build**.
No live xnatR archive downloads or authenticated acceptance run have occurred.

## Next steps

1. Run the prepared Terminal launcher with `--plan-only` and enter credentials
   locally. It verifies the saved inventory receipt and rechecks live source
   membership, metadata and catalogs while planning. The all-project discovery
   component remains unqualified.
2. Review the saved DICOM plan, catalogs, counts, sizes and separate destinations.
3. Complete one resource, interrupt another, and restart the same full plan in a
   separate destination; prove verified resources are not downloaded again.
4. Require whole-selection completeness and an all-reuse rerun.
5. Record qualification evidence, commit any fixes and prepare the release or PR.

The [qualification receipt](QUALIFICATION-2026-11-04.md) records the source and
artifact hashes, isolated environment, evidence paths and launcher. Live steps
2-6 remain open. The existing downloader and its artifacts remain untouched.

## Historical roadmap

The January 2026 phase checklist and metrics in [ROADMAP.md](ROADMAP.md) are
historical and have not been reconciled requirement by requirement. Local delivery
of the three current workflow tranches is not a claim of complete CRAN or broader
roadmap qualification.

## Session continuity

Last activity: resumed the saved qualification sequence, fixed and regression-tested
source-change stopping, committed the implementation, built/installed the exact
source and validated it. The
discovery launcher initially failed before login because its processx child could
not open `/dev/tty`; the callback incorrectly returned cancellation. The repaired
launcher passed five synthetic-terminal tests, including hidden password entry,
interruptions and distinct missing-terminal errors. Its command path is unchanged.
The collector fix is committed, built, installed and locally validated. The
launcher prints source `96fc105` and `--plan-only` retains the inventory's original
`6e80224` provenance. A checked compatibility record confirms the inventory
function and all 274 retained collection routes are unaffected by the file-catalog
change. Planning still rechecks the live source under the new revision. The
original all-project HTTP 403 remains unresolved. No live plan or archive
execution has completed. No push, PR or release has occurred.

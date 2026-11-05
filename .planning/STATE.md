# Project State

Updated: 2026-11-04.

## Current focus

The three discovery-to-verified-download implementation tranches are frozen at
`6e80224a001027fad4fcbacbd311bd5c4a9452d4` and installed in an isolated library.
Installed-package validation: 775 passing assertions, one real-fzf skip, no test
failures/errors/warnings; package check Status: OK with tests run separately.
Source changes now stop execution even with `continue_on_error = TRUE`.

Step 1 of [LIVE-QUALIFICATION.md](LIVE-QUALIFICATION.md) is complete. Step 2 has
passed credential-free system-curl connectivity with TLS enabled and HTTP 401;
httr2 still reports a network failure. User-entered credentials now successfully
authenticate R, but all-project discovery stopped with HTTP 403. Its original
failure record lacks an endpoint, so the denied request remains unidentified.
The next gate is **complete authenticated discovery**, followed by the frozen
eight-session plan.
No live xnatR archive downloads or authenticated acceptance run have occurred.

## Next steps

1. Run the prepared Terminal discovery launcher and enter credentials locally.
   Discover through R/system curl within the owner-confirmed project and compare
   the exact selection. The all-project discovery component remains unqualified.
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
The next user attempt authenticated but discovery returned HTTP 403. The launcher
now uses the already confirmed project by default and saves private endpoint/status
diagnostics for any failure. Synthetic tests verify that unrelated-project denials
do not affect this explicit scope, while denials inside it still abort. No live
inventory or plan has completed. No push, PR or release has occurred.

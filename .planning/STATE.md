# Project State

Updated: 2026-11-04.

## Current focus

The discovery-to-verified-download implementation has completed three local
tranches: transport/authentication, inventory/planning, and verified execution
with restart. The next gate is **live qualification of the actual eight-session
job**, following [LIVE-QUALIFICATION.md](LIVE-QUALIFICATION.md).

Local validation: 757 passing assertions, one real-fzf test skipped, and final
package check Status: OK with tests run separately. Implementation and subsequent
planning changes remain uncommitted. No authenticated live acceptance run has
been completed with this code, and the existing downloader was left untouched.

## Next steps

1. Freeze the implementation in a local commit and install it into an isolated R library.
2. Diagnose connectivity before credentials, then discover through R/system curl
   and compare with the existing owner-confirmed eight-session selection.
3. Review and save the exact DICOM plan, catalogs, counts, sizes and destinations.
4. Complete one resource, interrupt another, and restart the same full plan in a
   separate destination; prove verified resources are not downloaded again.
5. Require whole-selection completeness and an all-reuse rerun.
6. Record qualification evidence, commit any fixes and prepare the release or PR.

All six live-qualification steps are pending. The detailed plan defines evidence,
source-change refusals, protection of the existing download, and privacy scope.

## Historical roadmap

The January 2026 phase checklist and metrics in [ROADMAP.md](ROADMAP.md) are
historical and have not been reconciled requirement by requirement. Local delivery
of the three current workflow tranches is not a claim of complete CRAN or broader
roadmap qualification.

## Session continuity

Last activity: user requested that shared memory and the repository plan capture
the live-qualification sequence. Resume at step 1 of LIVE-QUALIFICATION.md.
This handoff records a plan; it does not launch the live job.

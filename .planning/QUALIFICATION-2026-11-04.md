# Source freeze and local qualification receipt

Recorded: 2026-11-04. Authenticated live acceptance remains open.

## Frozen implementation

- Implementation commit: `6e80224a001027fad4fcbacbd311bd5c4a9452d4`.
- Package: xnatR 0.2.0, built from a `git archive` of that exact commit.
- Built package SHA-256:
  `cead4c294fde84ef6016ab7a0dec17c11fa5ddd63a90cc6ecf0a5bcbb92b8c75`.
- Artifact: `/private/tmp/xnatr-live-qualification-20261104/build/xnatR_0.2.0.tar.gz`.
- Dedicated library: `/private/tmp/xnatr-live-qualification-20261104/library`.
- R: 4.6.1, aarch64-apple-darwin23; system curl 8.7.1; unzip 6.00.
- The library contains the tested xnatR plus copies of 55 available non-base
  dependency packages. Only that library and the framework library are used.
  Exact dependency versions and library paths are in `evidence/environment.json`
  under the workspace below. The default user library was not modified.

Later documentation commits record this result; they do not change which source
revision or package artifact was tested.

## Review fix and validation

The original 757-assertion suite passed with one fzf skip. A new synthetic witness
then showed that `continue_on_error = TRUE` downloaded the next resource after a
source catalog changed during transfer. That contradicted the saved qualification
rule. The executor now always stops on `source_changed`; ordinary transfer failures
may still continue when explicitly requested.

The failing witness recorded two archive GETs and states `source_changed,
downloaded`. After the fix it recorded one GET and states `source_changed, pending`.
Regression tests cover source-change refusal on both backends and continuation
after an ordinary truncated transfer.

- Tests on the installed frozen artifact: **775 passed, one fzf skip, zero
  failures, errors or warnings**.
- `R CMD check --no-manual --no-tests`: **Status: OK**. Tests were run separately
  with `testthat::test_package()` from the isolated installation.
- Both vignettes built and their outputs passed package checks.
- Optional getPass is not installed. The credential callback uses Python
  `getpass` through `/dev/tty`; see the tested launcher repair below.

Evidence workspace: `/private/tmp/xnatr-live-qualification-20261104`.
Retained files include `evidence/development-tests.log`, `source-change-before.log`,
`source-change-after.log`, `installed-tests.log`, `installed-test-counts.json`,
`build.log`, `install.log`, `check.log`, `environment.json` and `connectivity.json`.
Initial dependency-path failures are recorded in `environment-preflight.txt`.
These are local temporary paths, not published artifacts.

## Live boundary and next action

Credential-free diagnostics using the frozen installed package reached the server
through system curl with TLS verification enabled and HTTP 401. httr2 reported a
network failure. No root cause or authenticated access is established by this test.

Run this prepared launcher in macOS Terminal and enter credentials there:

```sh
/private/tmp/xnatr-live-qualification-20261104/discover.command
```

It reads the existing private reference selections, checks their agreement,
discovers date-filtered sessions across accessible projects through R, compares
canonical session/subject associations, and creates a plan for a separate private
destination. It does not start archive transfers. Credentials are not written to
files or command arguments. Review the exact saved plan before proceeding through
the interruption/restart and completeness steps in LIVE-QUALIFICATION.md.

The historical downloader's private summary was read and reports completion for
eight sessions and 241 scans. Its files and processes were not modified, and its
summary is not evidence that the new xnatR workflow passed live qualification.

## Terminal launcher repair

The first user-run launcher stopped with `xnatR_cancelled` before login. A
synthetic controlling-terminal reproduction, also run outside the sandbox,
confirmed that `processx::run(..., stdin = "/dev/tty")` failed to launch the child
with `Device not configured`. The callback swallowed that error and returned
NULL, incorrectly classifying an infrastructure failure as user cancellation.

The operational launcher now captures the Python helper through
`system2(stdout = TRUE)`, retaining access to the controlling terminal. Credentials
travel through the captured pipe. The helper refuses any getpass fallback that
could echo a password. Empty input and Ctrl-C remain cancellation; unavailable
terminal access now raises `xnatR_prompt_error` with an actionable message. The
shell launcher also preserves R's exit status.

Five synthetic-terminal tests passed: successful credentials, empty username,
empty password, interruption during password entry, and no controlling terminal.
Tests verified disabled password echo, absence of the dummy password in terminal
output, and restored echo afterward. Tests used the frozen installed package's
actual credential callback boundary without live credentials or network access.
Both R scripts parse and the shell syntax check passes.

The launcher path is unchanged. Evidence and the original launcher are retained
under `evidence/prompt-repair/` in the local workspace. This repair changes the
operational launcher only; the frozen package commit and 775-assertion result
remain unchanged. Authenticated live discovery still awaits a successful login.

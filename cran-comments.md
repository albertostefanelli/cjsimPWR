## Submission

This is the first CRAN submission of cjsimPWR, version 0.4.0. Earlier versions were distributed on
GitHub; no earlier version was submitted to CRAN.

## Test environments

The local check was run on 2026-10-16.

* local: macOS 13.7.8 (aarch64-apple-darwin21.5.0), R 4.2.1
* GitHub Actions: macOS, R release
* GitHub Actions: Windows, R release
* GitHub Actions: Ubuntu, R release, R-devel and R oldrel-1

The GitHub Actions results for the submitted source commit are recorded here once the release is
pushed; see "Before submission" below.

The local check used `R CMD check --as-cran --run-donttest`, including PDF manual generation, with
`_R_CHECK_SYSTEM_CLOCK_=false`, since R 4.2.1 could not reach its remote time service. The local
clock was verified against the CRAN HTTPS response `Date` header, agreeing to the second; only the
remote clock probe was disabled, and the future-file-timestamp check remained enabled and passed.
The CI checks use `--no-manual --as-cran --run-donttest`.

## R CMD check results

Local check: 0 errors | 0 warnings | 1 note

* CRAN incoming feasibility identifies a new submission. It also reports three address checks:
  * HTTP 403 for DOI `10.1177/1745691614551642`, Gelman and Carlin (2014), "Beyond Power
    Calculations". The DOI resolves through the doi.org handle service to the SAGE article page,
    which denies the automated request from the check environment. The valid DOI has been retained.
  * HTTP 503 for DOI `10.31235/osf.io/spkcy`, Stefanelli and Lukac (2020), in DESCRIPTION,
    `inst/CITATION` and README. The DOI resolves through the doi.org handle service; osf.io was
    unavailable for scheduled maintenance when the check ran, and its home page returned the same
    status. The valid DOI has been retained.
  * HTTP 404 for `https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/paired_logit.md`,
    linked from the package help and README. The guide is part of the source commit and resolves
    once the release is merged into the repository's `main` branch.

## Notes for the reviewer

* Random numbers. `power_sim()` requires an explicit `seed`, starting one L'Ecuyer-CMRG stream per
  simulated experiment so results do not depend on worker count. Calibration is seeded separately by
  `calibration_control$seed`, and the population targets of `utility_model()` by `control$seed`
  (both documented, default 104729), so the same inputs always define the same population and
  reported true effects. The caller's `.Random.seed` and `RNGkind()` are saved on entry and restored
  on exit; `.Random.seed` is the only object the package writes to the global environment, and only
  for the call's duration. `simulate_experiment()` draws the experiment itself from the caller's
  stream, as `sample()` does.
* Parallel computation. `power_sim()` uses one core unless `cores` is set; the examples use one
  core.
* Suggested package. `clubSandwich` is used only when `vcov = "CR2"` is requested; without it, that
  request gives an informative error. No example requires it.

## Before submission

Remove this section once its items are done.

1. Push the release and merge it into `main`, so that the paired-logit guide URL resolves; rerun the
   URL check afterwards.
2. Record the GitHub Actions run for the submitted commit, with all five jobs passing, under "Test
   environments".
3. Confirm that osf.io is back and the DOI check passes, or mention the maintenance window in the
   submission comment.

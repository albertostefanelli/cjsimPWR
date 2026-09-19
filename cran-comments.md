## Submission

This is the first CRAN submission of cjsimPWR, version 0.3.0. Earlier versions were distributed on
GitHub.

## Test environments

The local check was run on 2026-09-19; the GitHub Actions checks were run on 2026-09-18.

* local: macOS 13.7.8 (aarch64-apple-darwin21.5.0), R 4.2.1
* GitHub Actions: macOS, R release
* GitHub Actions: Windows, R release
* GitHub Actions: Ubuntu, R release, R-devel and R oldrel-1

All five GitHub Actions jobs passed for source commit `b33b9d2`:
https://github.com/albertostefanelli/cjsimPWR/actions/runs/35408165008

The local check used `R CMD check --as-cran --run-donttest`, including PDF manual generation, with
`_R_CHECK_SYSTEM_CLOCK_=false`. R 4.2.1 could not reach its remote time service, worldtimeapi.org.
The local clock was independently verified against the CRAN HTTPS response timestamp and timeapi.io;
both agreed within one second. Only the remote clock probe was disabled. The check for future file
timestamps remained enabled and passed.
The CI checks used `--no-manual --as-cran --run-donttest`.

## R CMD check results

Local check: 0 errors | 0 warnings | 1 note

* CRAN incoming feasibility identifies a new submission. It also reports HTTP 403 after
  `https://doi.org/10.1177/1745691614551642` redirects to the SAGE article page. Crossref confirms this
  DOI for Gelman and Carlin (2014), "Beyond Power Calculations". The publisher denies the automated
  request from the local check environment; the valid DOI has been retained.

The published documentation URLs are accessible, and a separate `urlchecker::url_check()` passed.

## Notes for the reviewer

* Random numbers. `power_sim()` has no default seed: the caller supplies `seed`, which starts one
  L'Ecuyer-CMRG stream per simulated experiment so that results do not depend on the number of
  workers. The calibration of the data-generating model is seeded by `calibration_control$seed`, a
  documented setting (default 104729), so that the same inputs always define the same population and
  the same reported true effects. In both cases the caller's `.Random.seed` and `RNGkind()` are saved
  on entry and restored on exit, as `withr::with_seed()` does. `.Random.seed` is the only object the
  package writes to the global environment, and only for the duration of the call.
  `simulate_experiment()` draws the experiment from the caller's stream, as `sample()` does.
* Parallel computation. `power_sim()` uses one core unless `cores` is set; the examples use one core.

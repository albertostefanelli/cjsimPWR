## Submission

This is the first CRAN submission of cjsimPWR, version 0.3.0. Earlier versions were distributed on
GitHub.

## Test environments

Checks recorded on 2026-09-18:

* local: macOS 13.7.8 (aarch64-apple-darwin21.5.0), R 4.2.1
* GitHub Actions: macOS, R release
* GitHub Actions: Windows, R release
* GitHub Actions: Ubuntu, R release, R-devel and R oldrel-1

All five GitHub Actions jobs passed for source commit `691a387`:
https://github.com/albertostefanelli/cjsimPWR/actions/runs/35407136812

The local check used `R CMD check --as-cran --run-donttest`, including PDF manual generation.
The CI checks used `--no-manual --as-cran --run-donttest`.

## R CMD check results

Local check: 0 errors | 0 warnings | 2 notes

* CRAN incoming feasibility identifies a new submission and reports HTTP 403 for
  DOI `10.1177/1745691614551642`. This is the valid DOI for Gelman and Carlin (2014), verified against
  Crossref and the publisher's article record. Automated requests from the local check environment
  are denied access; the citation has been retained.
* The local environment reports `unable to verify current time` when checking timestamps.

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

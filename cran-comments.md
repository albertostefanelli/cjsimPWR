## Before submitting (delete this section)

* TODO: after the push to main, record the GitHub Actions results below.
* TODO: run `devtools::check_win_devel()` and record the result below.
* TODO: confirm that every environment reports only the new-submission note.

## Test environments

* local: macOS 13.7 (aarch64-apple-darwin21.5.0), R 4.2.1
* GitHub Actions: macOS and Windows (R release); Ubuntu (R devel, release and oldrel-1)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

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

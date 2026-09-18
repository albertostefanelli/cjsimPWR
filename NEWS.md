# cjsimPWR 0.3.0

This release substantially rewrites the simulation, estimation and reporting code. Power values differ slightly from those of earlier versions, the preprint and the Shiny app, which used the
previous simulation model; the changes that affect results are listed under "Simulation model" and
"Performance measures" below.

## Simulation model

* Choices come from a logit model calibrated so that the population AMCEs match the requested ones,
  instead of a rule that clipped the odds. In 0.2.1, requested AMCEs of 0.10 and 0.20 gave 0.106 and
  0.209 with two binary attributes.
* Both profiles of a task are always drawn independently, as in a fully randomized design. Version
  0.2.1 removed identical pairs, which inflated the estimator's target by M/(M - 1), where M is the
  number of possible profiles.
* `sigma` is the standard deviation of respondent-level AMCEs on the probability scale. Levels with
  a requested AMCE of zero stay zero in the population; individual effects can vary. `latent_sigma`
  is available for users who think on the coefficient scale.
* `dgp = "linear"` gives exact AMCEs without heterogeneity; `dgp = "odds"` keeps the 0.2.1 choice
  rule for comparison and is deprecated.
* Any number of attributes is supported, with names and level labels. In 0.2.1, designs with ten or
  more attributes attached effects to the wrong attributes (`var_10` was read before `var_2`).
* Subgroup coefficients are matched to groups by name. In 0.2.1 they were assigned by alphabetical
  group order, so `group_name = c("Z", "A")` swapped the groups' effects.
* Each calibrated model is verified against the requested AMCEs and `sigma`, then recalculated once
  with fresh draws and a budget fixed in advance, so that the reported values are not selected by the
  acceptance check. This final reference supplies the `truth` table and its Monte Carlo precision,
  including for differences between groups that request equal AMCEs. Both calculations are kept in
  `diagnostics[[g]]$verification` and `diagnostics[[g]]$reference`.
* A warning is issued only if the final reference contradicts a requested AMCE or `sigma`, that is, if
  an interval lies wholly outside its tolerance band (`$reference$contradicted`). An inconclusive
  recheck is recorded quietly (`$reference$accepted = FALSE`). The model is kept in both cases.
* Optional `calibration_control = list(reference_margin = 0.5)` reserves half the tolerance during
  verification and plans the independent final batch count from inflated verification variances.
  `max_reference_batches` caps that count (default 512 per group); `reference_plan` records predictions,
  cap hits and realised precision. This experimental option is off by default; it does not guarantee
  confirmation and can increase calibration work or fail its stricter verification.
* The deprecated odds model has score inputs rather than AMCE targets, so verification checks numerical
  precision only. Each retry doubles `verification_pairs` and `verification_draws`, up to
  `max_attempts`; an exhausted budget gives a specific error.

## Estimation and inference

* AMCEs are estimated by least squares with respondent-clustered standard errors computed with
  `sandwich::vcovCL(type = "HC1", cadjust = TRUE)`, the estimator of `cjoint::amce(cluster = TRUE)`.
  Normal critical values are the default; `inference = "t"` and `vcov = "CR2"` with Satterthwaite
  degrees of freedom (via `clubSandwich`) are available. See the [clustering guide](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/clustering.md).
* With subgroups, `power_sim()` also reports the power to detect differences between each group's
  AMCEs and those of the first group.
* `alpha` sets the significance level.
* Perfectly fitted subgroups report failed inference (`invalid_variance`) even when other groups have
  residual variation. Roundoff standard errors are not treated as valid subgroup results; positive
  variances of other effects and contrasts are retained.

## Performance measures

* The default printout includes Type I error and its Monte Carlo standard error for null targets.
  `null_status` distinguishes exact and calibrated (approximate) nulls from non-null targets; the latter
  report power. Calibrated nulls display `target_error_bound`, a reference confidence bound. All null
  targets, exact and calibrated, omit empirical and analytic Type S/M. Odds models identify nulls from
  their calculated AMCEs.
* Bias, coverage and Type S/M are computed against the numerically calculated population AMCE
  (`true_amce`). `bias_mcse` includes the uncertainty of that reference; coverage MCSE is conditional on
  it, and `coverage_reference_lower` and `coverage_reference_upper` show how much it could matter.
  `summarise_runs()` accepts the corresponding optional `null`, `reference_mcse` and
  `reference_half_width` arguments.
* For calibrated nulls, `null_size_sensitivity` shows how much the residual calibration error could raise
  the rejection rate under a known-SE normal benchmark, and a warning flags cases where it may matter.
  It is a diagnostic, not a guarantee about the size of the clustered test.
* Type S and Type M errors follow Gelman and Carlin (2014): Type M is the mean of
  `|estimate| / |true AMCE|` over significant runs. Version 0.2.1 averaged the signed ratio against
  the input, which understated Type M at low power, and reported Type S = 1 and an undefined Type M
  for zero effects.
* Every measure comes with a Monte Carlo standard error, and results are numeric rather than
  formatted text. The number printed in parentheses in 0.2.1 was the standard deviation of a 0/1
  indicator, not a precision measure.
* Coverage is computed over all valid runs. Failed runs are counted (`n_failed`) with their reasons
  (`$failures`), never silently dropped or treated as nonsignificant.
* `sim_runs` defaults to 1000.

## Interface

* New arguments: `levels`, `true_amce`, `groups`, `sigma`, `dgp`, `alpha`, `vcov`, `inference`,
  `cores`, `keep_runs`, `latent_sigma`, `calibration_control`.
* Deprecated arguments still work with a warning: `n_levels` (`levels`), `true_coef` (`true_amce`),
  `group_name` (`groups`), `sigma.u_k` (`sigma`, or `latent_sigma` with `dgp = "odds"`) and
  `n_attributes`. Positional calls must be migrated to named arguments.
* `power_sim()` returns a `cj_power` list with `performance`, `truth`, `parameters`, `diagnostics`,
  `settings`, `failures`, optional `runs` and the calibrated `model`.
* New exported building blocks: `conjoint_design()`, `simulate_experiment()`, `estimate_amce()` and
  `summarise_runs()`.
* Removed: `generate_design()`, `generate_samples()`, `simulate_conjoint()`, `sim_to_long()`,
  `evaluate_model()` and `sim_cj()`. Use `conjoint_design()` and `simulate_experiment()` to simulate
  a data set, `estimate_amce()` to estimate it, and `summarise_runs()` to summarise repeated runs.
* Parallel runs use base R workers (`cores`) with one random stream per run: results are identical for
  any number of cores, and the caller's random-number state and `future` plan are left unchanged.
* Runtime dependencies are now `stats`, `parallel` and `sandwich`; `dplyr`, `marginaleffects`,
  `future`, `future.apply`, `progressr`, `purrr`, `stringr` and `tibble` are no longer required.

## Package documentation

* `?cjsimPWR` provides a package overview and links to the main functions.
* Release notes ship with the package and are available through `news(package = "cjsimPWR")`.

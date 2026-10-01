# cjsimPWR 0.3.0

This release substantially rewrites the simulation, estimation and reporting code. Results can differ
from those of earlier versions, the preprint and the Shiny app, which used the previous simulation
model; the changes that affect results are listed under "Simulation model" and "Performance measures"
below.

## Simulation model

* Choices come from a logit model calibrated so that the population AMCEs match the requested ones,
  instead of a rule that clipped the odds. In 0.2.1, requested score coefficients of 0.10 and 0.20 gave
  generated AMCEs of about 0.106 and 0.209 for two binary attributes; reproduce with
  `dgp = "odds"`, e.g. `simulate_experiment(conjoint_design(c(2, 2)), list(0.10, 0.20), units = 2000,
  n_tasks = 1, dgp = "odds")`, and inspect `attr(data, "dgp")$truth$true_amce`.
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
* Each calibrated model is verified against the requested AMCEs and `sigma` with a fresh, independent
  final reference that supplies the `truth` table and is never selected by the acceptance check. A
  warning is issued only if that reference contradicts a request; an inconclusive recheck is recorded
  quietly, without a warning. Optional, experimental `calibration_control = list(reference_margin = 0.5)`
  (off by default) can reduce inconclusive checks at the cost of more calibration work, without
  guaranteeing confirmation. See the
  [calibration guide](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/calibration.md) for
  the full mechanics, diagnostic fields and schema.
* The deprecated odds model has score inputs rather than AMCE targets, so its verification checks
  numerical precision only, retrying with larger samples up to a fixed attempt budget; exhausting it
  gives a specific error.

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
  Non-null targets report power. All null targets omit empirical and analytic Type S/M. Odds models
  identify nulls from their calculated AMCEs.
* Bias, coverage and Type S/M are computed against the numerically calculated population AMCE
  (`true_amce`). `bias_mcse` includes the uncertainty of that reference; coverage MCSE is conditional on
  it, and `coverage_reference_lower` and `coverage_reference_upper` show how much it could matter.
  `summarise_runs()` accepts the corresponding optional `null`, `reference_mcse` and
  `reference_half_width` arguments.
* An internal normal-benchmark check warns when approximate nulls may be sensitive to calibration
  error. Its calculation and threshold are unchanged; `null_status`, `null_size_sensitivity` and the
  calibrated print decoration have been removed from the unreleased interface. Numerical truth and
  reference uncertainty remain available. The warning is not a guarantee about clustered inference.
* Clarified that profile enumeration depends on the profile-pair count, not on heterogeneity;
  heterogeneous coefficients still require Monte Carlo integration.
* Type S and Type M errors follow Gelman and Carlin (2014): Type M is the mean of
  `|estimate| / |true AMCE|` over significant runs. Version 0.2.1 averaged the signed ratio against
  the input, which understated Type M at low power, and reported Type S = 1 and an undefined Type M
  for zero effects.
* Every empirical rate, bias and standard-error measure reports its own Monte Carlo standard error
  (`_mcse` columns; not every returned field has one — see `summarise_runs()` help), and results are
  numeric rather than formatted text. The number printed in parentheses in 0.2.1 was the standard
  deviation of a 0/1 indicator, not a precision measure.
* Coverage is computed over all valid runs. Failed runs are counted (`n_failed`) with their reasons
  (`$failures`), never silently dropped or treated as nonsignificant.
* `sim_runs` defaults to 1000.

## Interface

* New arguments: `levels`, `true_amce`, `groups`, `sigma`, `dgp`, `alpha`, `vcov`, `inference`,
  `cores`, `keep_runs`, `latent_sigma`, `calibration_control`.
* Deprecated arguments still work with a warning, and positional calls must be migrated to named
  arguments:

  | Deprecated | Replacement | Notes |
  | --- | --- | --- |
  | `n_levels` | `levels` | |
  | `true_coef` | `true_amce` | Default DGP is now calibrated logit; use `dgp = "odds"` to interpret it as legacy score coefficients. |
  | `group_name` | `groups` | |
  | `n_attributes` | (checked against `levels`) | |
  | `sigma.u_k` | `sigma` (default DGP), or `latent_sigma` with `dgp = "odds"` | Reproduces only that legacy parameter's interpretation, not a full 0.2.1 experiment: profile pairs are always drawn independently now (see "Simulation model" above). |
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

# The simulation model

`power_sim()` and `simulate_experiment()` generate forced-choice conjoint data whose population AMCEs
match the requested values within a calibration tolerance. This page explains the model, its
assumptions and its calibration; for the full calibration mechanics and precision controls, see the
[calibration guide](calibration.md).

## Choice model

Each respondent `i` has a utility coefficient for every non-reference level of every attribute. In a task
with profiles 1 and 2, the respondent chooses profile 1 with probability

```
P(choose 1) = logistic( U_i(profile 1) - U_i(profile 2) )
```

where `U_i` is the sum of the respondent's coefficients for the levels shown. Both profiles are drawn
independently and uniformly over all attribute levels, as in the fully randomized design of Hainmueller,
Hopkins and Yamamoto (2014), and the two profiles of a task always receive complementary outcomes.

The AMCE of a level is the average change in the probability that a profile is chosen when that level
replaces the reference level, averaged over the other attributes, the competing profile and the
respondents. Because the choice model is nonlinear, the utility coefficients are not AMCEs, and (with
heterogeneity) an individual respondent's AMCE need not be normally distributed even though the
underlying utility deviations are Gaussian. The package therefore solves for coefficients whose
population AMCEs match the requested ones (the `true_amce` argument) within a small tolerance (see
[Calibration](#calibration) below):

```r
library(cjsimPWR)

design <- conjoint_design(c(2, 3, 5))
amce   <- list(0.05, c(-0.05, 0.05), c(-0.03, -0.05, -0.05, 0.05))
set.seed(1)
data   <- simulate_experiment(design, amce, units = 500, n_tasks = 5, sigma = 0.05)

attr(data, "dgp")$truth      # requested and true AMCEs, and the SD of respondent-level AMCEs
```

`dgp = "logit"`, the default, names this choice model. The argument is retained so that later versions
can add other choice models without changing existing calls.

## Assumptions and feasibility

Power is computed for data generated under these assumptions; departures in a real study change it.

- **Design:** paired forced choice only; ratings, opt-outs and larger choice sets are not simulated.
  Both profiles of every task are drawn independently and uniformly over attribute levels.
- **Stable choices:** given a respondent's preferences, tasks are independent, with no carryover,
  fatigue or order effects.
- **Sampling:** respondents are a simple random sample; higher-level clusters such as countries are not
  simulated (see [clustering](clustering.md)). Task counts are constant within a group, though they may
  differ between groups.
- **Effects:** utilities are additive across attributes, with no requestable interactions between
  attributes. This is distinct from the probability scale, where the logit link makes AMCEs interact
  even though the underlying utilities do not.
- **Heterogeneity:** deviations are independent across attributes, so a respondent who dislikes a level
  of one attribute is no more likely to dislike a level of another. `groups` models specified,
  requested differences between named subgroups; it does not represent general correlated preferences
  shared across attributes (for example through ideology) within one population.

Not every request is feasible. For an attribute with L levels, no AMCE can reach
`1 - 1/L` (0.5 for two levels), `AMCE^2 + sigma^2` must stay below `(1 - 1/L)^2`, and the marginal choice
probabilities implied by an attribute's AMCEs must lie within (0, 1). These are **necessary** conditions,
checked before solving, not a full feasibility proof: passing them does not guarantee a solution exists.
Separately, the iterative solver can fail to converge; that is numerical non-convergence, not a proof
that the requested population cannot exist (it may still indicate infeasibility or a boundary target).
Either failure stops the call with an error.

## Heterogeneity: `sigma`

`sigma` is a single non-negative number: the standard deviation, across respondents, of each
respondent's own AMCE, on the probability scale, shared across every effect and every group in one call.
`sigma = 0` gives every respondent the same AMCE. A level with a requested AMCE of zero has an average
effect of exactly zero in the population, even though individual
effects still vary around zero with SD `sigma`; Type I error refers to this population-average null.

The package draws Gaussian deviations of the utility coefficients, calibrated so that respondent-level
AMCEs have SD `sigma`. `latent_sigma` instead fixes the SD of the utility deviations directly and reports
the implied AMCE SDs (`amce_sd` in the `truth` table); only one of the two can be given. The deprecated
`sigma.u_k` of versions up to 0.2.1 is treated as `sigma`.

There is no generally valid value of `sigma`. Without pilot data, compare separate calls at several
values while keeping the requested AMCEs fixed; `sigma` barely affects power when each respondent
completes one task, and matters more the more tasks they complete (see rows 5 to 7 of the
[closed-form comparison](closed_form.md) and [clustering](clustering.md)):

```r
sigmas <- c(0, 0.05, 0.10, 0.15)
power  <- sapply(sigmas, function(s) power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500,
                                               n_tasks = 5, sigma = s, seed = 1)$performance$power)
colnames(power) <- sigmas
rownames(power) <- with(power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500, n_tasks = 5,
                                  sigma = 0, seed = 1)$performance, paste(attribute, level, sep = ":"))
power
```

## Calibration

Calibration exists so that a nonlinear choice model can still be told "AMCE of 0.05," not just a utility
coefficient. It runs once per group, in three stages: **calibrate** (solve for
coefficients reproducing the requested AMCEs and `sigma`), **verify** (check the result against fresh,
independent draws at a nominal 99% Monte Carlo confidence, retrying with more draws or recalibrating if
needed), and compute a **final reference** (an independent recalculation, on a fixed budget, that never
feeds back into calibration). The default tolerance is 0.001 on the probability scale. Its effect on
power depends on the requested effect and its sampling standard error: even a small AMCE error can
matter near a study's power threshold. For example, at a fixed standard error of 0.02, changing an AMCE
from 0.05 to 0.0494 changes two-sided normal-test power at the 5% level from about 70.5% to 69.5%.
This is a sensitivity calculation, not an exact prediction for a fitted clustered test. Inspect
`target_error_bound` relative to the study's standard error and tighten calibration controls when that
sensitivity matters. Increasing `sim_runs` alone does not reduce calibration error. Calibration takes
seconds for small designs but can take many minutes when `sigma > 0` and profiles must be sampled.
See the [calibration guide](calibration.md) for the full mechanics, diagnostics and precision controls,
and the `simulate_experiment()` help for the parameter contract.

## Inspecting and reusing a model

The `truth` table (`attr(data, "dgp")$truth`, or `power_sim()`'s `$truth`) has one row per effect and
group, including differences between groups: `requested_amce`, `true_amce` (the numerical reference for
the generated population's AMCE — see the [calibration guide](calibration.md#1-targets-and-numerical-truth)
for the distinction between requested, generated and reference values), `amce_sd`, reference precision,
and `target_error_bound`. A calibrated model can be reused without recalibration, provided
the design matches exactly and, with groups, the group order matches the prepared model's:

```r
model <- attr(data, "dgp")   # or power_sim()'s $model
set.seed(2)
more  <- simulate_experiment(design, units = 500, n_tasks = 5, model = model)
```

## References

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis:
Understanding multidimensional choices via stated preference experiments. *Political Analysis*,
22(1), 1–30. <https://doi.org/10.1093/pan/mpt024>

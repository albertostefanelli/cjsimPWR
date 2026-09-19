# The simulation model

`power_sim()` and `simulate_experiment()` generate forced-choice conjoint data whose population AMCEs
match the requested values within a calibration tolerance. This page explains the model and its precision.

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
respondents. Because the choice model is nonlinear, the utility coefficients are not AMCEs. The package
therefore solves for coefficients whose population AMCEs match the requested ones (the `true_amce`
argument) within a small tolerance (see "Calibration" below):

```r
library(cjsimPWR)

design <- conjoint_design(c(2, 3, 5))
amce   <- list(0.05, c(-0.05, 0.05), c(-0.03, -0.05, -0.05, 0.05))
data   <- simulate_experiment(design, amce, units = 500, n_tasks = 5, sigma = 0.05)

attr(data, "dgp")$truth      # requested and true AMCEs, and the SD of respondent-level AMCEs
```

`dgp` selects the model. `"logit"` (the default) is the model above. `"linear"` sets
`P(choose 1) = 0.5 + tau(profile 1) - tau(profile 2)`, where `tau` sums the requested AMCEs; the AMCEs are
then exact by construction, but no heterogeneity is possible and the attribute ranges must sum to at most
0.5, otherwise the probabilities leave [0, 1]. `"odds"`, the deprecated model of versions up to 0.2.1,
takes score coefficients rather than AMCEs and is kept only to reproduce old results.

Not every request is feasible. In the logit model, for an attribute with L levels, no AMCE can reach
`1 - 1/L` (0.5 for two levels), `AMCE^2 + sigma^2` must stay below `(1 - 1/L)^2`, and the marginal choice
probabilities implied by an attribute's AMCEs must lie within (0, 1); otherwise the call stops with an
error.

## Heterogeneity: `sigma`

`sigma` is the standard deviation, across respondents, of each respondent's own AMCE, on the probability
scale. With `sigma = 0.05` and a requested AMCE of 0.03, individual AMCEs are centred on 0.03 with SD
0.05, so about 27% of respondents have a negative effect. Because `sigma` is absolute, the same value
implies more opposite-sign effects when the average effect is small. `sigma = 0` gives every respondent
the same AMCE.

The package draws Gaussian deviations of the utility coefficients, calibrated so that respondent-level
AMCEs have SD `sigma`. A level with a requested AMCE of zero has an average effect of exactly zero in the
population, while individual effects still vary around zero with SD `sigma`; Type I error refers to this
population-average null. The same `sigma` applies to every effect and every group, so one run cannot make
one attribute more divisive than another: set it for the attribute whose power matters most, or compare
runs. `latent_sigma` instead fixes the SD of the utility deviations and reports the implied AMCE SDs
(`amce_sd` in the `truth` table); only one of the two can be given. The deprecated `sigma.u_k` of versions
up to 0.2.1 is treated as `sigma`.

Individual AMCEs are approximately, not exactly, normal, because the map from utility coefficients to
AMCEs is nonlinear. For a requested AMCE of zero the distribution is exactly symmetric around zero.
For small effects and modest heterogeneity it can be close to normal. For example, in single-attribute
simulations with two or five levels and all other level targets set to zero, a requested AMCE of 0.03
and `sigma = 0.05` give about 27–28% of respondents a negative effect, against 27.4% under a normal
distribution. A left skew can appear when the requested positive AMCE and `sigma` are both large,
particularly for attributes with few levels, because no respondent's AMCE can reach `1 - 1/L` either.
In the same designs, a requested AMCE of 0.20 and `sigma = 0.20` give skewness of about -0.4 for a
five-level attribute and -0.8 for a binary one, with about 17–18% of respondents having a negative
effect, against 15.9% under a normal distribution. The shape also depends on the other attributes and
their effects. Calibration targets the requested population mean and SD using the separate AMCE and
SD tolerances described below.

There is no generally valid value of `sigma`. Without pilot data, compare runs with several values while
keeping the requested AMCEs fixed. `sigma` barely affects power when each respondent completes one task,
and matters more the more tasks they complete (see rows 5 to 7 of the
[closed-form comparison](closed_form.md) and [clustering](clustering.md)). Each call takes one value:

```r
sigmas <- c(0, 0.05, 0.10, 0.15)
power  <- sapply(sigmas, function(s) power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500,
                                               n_tasks = 5, sigma = s, seed = 1)$performance$power)
colnames(power) <- sigmas
power
```

## What the model assumes

Power is computed for data generated under these assumptions; departures in a real study change it.

- **Design:** paired forced choice only; ratings, opt-outs and larger choice sets are not simulated.
- **Stable choices:** given a respondent's preferences, tasks are independent, with no carryover,
  fatigue or order effects.
- **Sampling:** respondents are a simple random sample; higher-level clusters such as countries are not
  simulated (see [clustering](clustering.md)).
- **Effects:** utilities are additive across attributes, and interactions between attributes cannot be
  requested.
- **Heterogeneity:** deviations are independent across attributes, so a respondent who dislikes a level
  of one attribute is no more likely to dislike a level of another. Heterogeneity shared across
  attributes, for example through ideology, should be specified with `groups`.

## Calibration

For the logit model, the package fits the model in three steps, once for each group:

1. **Calibrate.** Solve for utility coefficients (and, with `sigma > 0`, the scale of their respondent
   deviations) that reproduce the requested AMCEs and `sigma`. AMCEs average over all possible profile
   pairs; these are enumerated exactly when the attributes being calibrated allow at most 10,000 pairs,
   and integrated by Monte Carlo otherwise. Without heterogeneity, attributes whose requested AMCEs are
   all zero need no calibration.
2. **Verify.** With fresh draws, check that every population AMCE lies within `tolerance` (default 0.001)
   of its request, and every AMCE SD within `min(0.005, 0.05 * sigma)` of `sigma`, with 99% Monte Carlo
   confidence. With groups, each AMCE is checked within half the tolerance, so differences between groups
   stay within `tolerance`. If the check fails, verification adds draws and, if needed, recalibrates with
   a larger training sample; if the targets still cannot be verified, the call stops with an error.
3. **Recalculate.** With a fresh random substream and a budget fixed in advance (by default, the
   verification's), calculate each AMCE and SD once more. This final reference supplies `true_amce` and
   its precision in the `truth` table. It is never retried or used to recalibrate, so the reported values
   are not selected by the acceptance rule. Exact enumeration without heterogeneity needs no fresh draws.

The default tolerance is far smaller than the standard error of an AMCE in a typical study (0.01 to
0.05), so calibration error does not materially affect power. Calibration takes seconds for small
designs but can take many minutes when `sigma > 0` and profiles must be sampled.

The final reference either confirms the tolerance, is inconclusive when an interval overlaps a tolerance
boundary (recorded quietly as `$reference$accepted = FALSE`), or contradicts it when an interval lies
wholly outside the band, `abs(estimate - target) - half_width > tolerance`. Only a contradiction produces
a warning (`$reference$contradicted = TRUE`); the model and reference are kept in every case.
`diagnostics[[g]]$verification` and `diagnostics[[g]]$reference` store the two calculations.

Calibration uses its own seed and leaves your random numbers unchanged. `calibration_control` overrides
the defaults, for example `list(tolerance = 0.0005)` for a tighter check; the settings and their defaults
are listed in `attr(data, "dgp")$control`. The experimental `reference_margin` setting reserves part of
each tolerance during verification and sizes the final reference so that it confirms the calibration
more often, at extra computational cost; it is off by default. For the deprecated odds model, whose
inputs are not AMCE targets, the checks concern numerical precision only. See
[options for greater reference precision](type_1_error.md#options-for-greater-reference-precision) and
[how the reference is calculated](type_1_error.md#how-the-reference-is-calculated).

## Inspecting and reusing a model

The `truth` table (`attr(data, "dgp")$truth`, or `power_sim()`'s `$truth`) has one row per effect and
group, including differences between groups: `input`, `requested_amce`, `true_amce` (the numerical
reference for the generated population's AMCE), `amce_sd`, reference precision, `null_status` and
`target_error_bound`. A calibrated model can be reused without recalibration:

```r
model <- attr(data, "dgp")   # or power_sim()'s $model
more  <- simulate_experiment(design, units = 500, n_tasks = 5, model = model)
```

## Reference truth and null targets

Null classification is separate from the numerical reference used for bias and coverage. Single-group
zeros in the calibrated models are exact; equal nonzero requests in separately calibrated populations
can leave a small residual contrast. See [Type I error and reference uncertainty](type_1_error.md) for
the reporting rules, confidence bounds, formulas, warnings, worked examples and references.

## References

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis:
Understanding multidimensional choices via stated preference experiments. *Political Analysis*,
22(1), 1–30.

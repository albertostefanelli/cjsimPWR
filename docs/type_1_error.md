# Type I error, null targets and reference uncertainty

cjsimPWR reports how often the planned analysis rejects the hypothesis that an effect is zero. When the
simulated effect is zero, that rate is the Type I error (false-positive) rate; otherwise it is power.
This guide explains how to read these results and the numerical checks behind them.

**In brief**

- Include a zero AMCE in the design to see the Type I error of the planned analysis next to power. At
  `alpha = 0.05` it should be close to 5%, allowing for its Monte Carlo standard error.
- A row reports Type I error when its requested AMCE, or requested difference between groups, is zero;
  otherwise it reports power. A nonsignificant estimate never turns power into Type I error.
- `null_status` says whether a zero target is `exact` or `calibrated`. A calibrated null, typically a
  difference between groups that request the same nonzero AMCE, is zero only up to a tiny calibration
  error, bounded by `target_error_bound`; its Type I error is approximate.
- `true_amce` is the package's numerical calculation of the AMCE in the simulated population. Bias and
  coverage are measured against it, and its own small uncertainty is carried into `bias_mcse`.
- After calibration, each AMCE and heterogeneity SD is recalculated independently. The package warns only
  if that recalculation contradicts the requested value; an inconclusive recalculation is recorded quietly.

Jump to: [checking an effect](#checking-the-type-i-error-of-an-effect),
[reporting rules](#which-rows-report-type-i-error), [examples](#examples-and-practical-use),
[reference calculation](#how-the-reference-is-calculated),
[precision options](#options-for-greater-reference-precision),
[calibration bounds and warnings](#the-bound-for-a-calibrated-null),
[coverage](#coverage-and-sensitivity-to-the-reference), or
[the closed-form comparison](#comparison-with-schuessler-and-freitag-2020).

## Why report Type I error alongside power?

For the two-sided hypothesis `H0: AMCE = 0`, Type I error is the probability of rejecting when the
population AMCE really is zero. Power is the probability of rejecting under a specified nonzero effect.
These describe repeated experiments under different population conditions. A nonsignificant estimate
does not establish a zero population effect, and significance does not determine which measure to report.

`alpha`, usually 0.05, is the nominal significance level. The actual rejection rate depends on how well
the estimator, standard errors and critical values work for the design. An analysis with underestimated
standard errors can report both greater power and too many false positives. Consequently, compare power
with null rejection rates, coverage and failed-run counts before interpreting greater power as better
performance. This joint assessment and the reporting of simulation uncertainty follow the principles of
[Morris, White and Crowther (2019)](https://doi.org/10.1002/sim.8086).

At a nominal 5% level, a well-calibrated test should have a null rejection rate near 5%, allowing for
Monte Carlo error; a conservative test can be below it.

### Checking the Type I error of an effect

To check a particular effect, run the simulation twice: first with its expected effect, to measure
power, and then with its target set to zero, to measure Type I error. Keep the other effect targets,
groups, sample sizes, tasks, heterogeneity and inference settings unchanged. A single run with every
target set to zero is a quick first check that reports a rate for all effects at once. The rate depends
largely on how many respondents inform each estimate, so check each subgroup and group difference, not
only the largest group; the [examples](#examples-and-practical-use) show both kinds of run and a
comparison across groups.

This check is particularly useful when respondents are few but complete many tasks, when a subgroup
is small despite a large overall sample, or when comparing inference methods. An apparently more
powerful analysis can also be more willing to report an effect that is absent. Check the relevant
null for each design or method before interpreting its higher power as an advantage. Small-sample
clustered inference can reject too often; see
[Pustejovsky and Tipton (2018)](https://doi.org/10.1080/07350015.2016.1247004) and the
[clustering guide](clustering.md).

For example, suppose education is expected to have an AMCE of 0.05 and experience an AMCE of 0.10.
Simulating those targets estimates the power to detect each effect. To check education's false-positive
rate, simulate education at zero while leaving experience at 0.10 and the remaining settings unchanged.
That second scenario asks how often the analysis reports an education effect when only experience has
a nonzero average effect. A simulation with both effects at zero answers a different, useful question:
how often each effect is falsely detected when neither has an average effect.

## Requested, generated and estimated effects

Three quantities must be distinguished. In short, `requested_amce` is what you asked for, and
`true_amce` is the package's calculation of what the simulated population actually has:

| Quantity | Meaning | Where to find it |
| --- | --- | --- |
| Requested effect, `tau` | The AMCE supplied by the user, or the difference between two requests. | `requested_amce` |
| Generated effect, `theta` | The population AMCE implied by the calibrated choice model. | Defined by the prepared model; not always available analytically. |
| Reference estimate, `theta_ref` | Exact integration or a numerical estimate of that generated AMCE. | `true_amce`, with `reference_mcse` and `reference_half_width` |

The name `true_amce` is retained in the output, but a Monte Carlo reference is an estimate of population
truth. There are two different sources of discrepancy: calibration can leave `theta` slightly different
from `tau`, and finite reference integration can leave `theta_ref` different from `theta`. Exact
integration removes the second source, not necessarily the first.

For example, two groups can both request an AMCE of 0.05 while their calibrated models generate slightly
different AMCEs. Subtracting the numerical references might give 0.00006 instead of zero. That difference
must be retained when evaluating bias and coverage. Replacing it with zero would score the estimator
against the request rather than the generated population, and setting its reference MCSE to zero would
hide numerical uncertainty. The importance of evaluating the generated estimand, and checking the precision
of a numerically estimated truth, is discussed by
[Naimi, Benkeser and Rudolph (2025)](https://doi.org/10.1097/EDE.0000000000001873).

In most applied cases, these differences are small enough to be negligible relative to the standard errors of the simulated experiments, so this distinction is not consequential. However, it is important to retain the distinction in the package's reporting and diagnostics for full transparency and reproducibility.

## Which rows report Type I error?

For the calibrated logit and linear models, `power_sim()` classifies a row as a null target when
`requested_amce == 0`. It uses the requested **contrast** for a difference row. There is no significance
test or rounding threshold in this classification. A small nonzero request still reports power.


| `null_status` | Interpretation | Rejection-rate column |
| --- | --- | --- |
| `exact` | Equality to zero follows from the model's structure. | `type_1_error` |
| `calibrated` | A null target without structural equality, usually equal nonzero requests in separately calibrated groups. | `type_1_error`, interpreted as approximate |
| `non-null` | A nonzero target, or a nonzero calculated AMCE in the odds fallback. (deprecated) | `power` |

The other rejection-rate column is `NA`. The default printout includes Type I error with its MCSE when
available, omits entirely unavailable measure columns, and shows `null_status`. For calibrated nulls it
also displays `target_error_bound` in scientific notation. A displayed `true_amce` rounded to 0.0000 is
not evidence of an exact null; inspect the status and unrounded numeric table.

### When is a null exact?

The calibrated models have four relevant exact cases:

1. **An individual AMCE requested as zero.** In the logit model, the null level and its reference have
   exchangeable utility distributions. Their population-average choice probabilities are equal.
2. **A zero contrast in the linear model.** The population AMCEs equal the supplied effects by construction.
3. **Identical populations reused across groups.** The same calibrated model and reference are shared,
   so the population difference and its reference error cancel exactly.
4. **A level with a zero AMCE in both populations.** Each component is structurally zero, even when the
   populations differ on other attributes.

These cases have zero reference MCSE and zero target error bound for the null. Equal *nonzero* requests
in otherwise different logit populations generally require separate calibration and are labelled
`calibrated`, even if their numerical discrepancy happens to be extremely small. In the legacy odds
model, a zero score with no coefficient heterogeneity also gives a structural zero; the calibrated
models' exchangeability argument should not be assumed for arbitrary heterogeneous odds inputs.

Heterogeneity does not contradict an exact population null. With a zero average AMCE and `sigma > 0`,
some respondents can have positive individual effects and others negative effects. Type I error concerns
the population-average null, not a claim that every respondent is unaffected. An all-zero average-effects
scenario can therefore still be heterogeneous.

### Why are Type S and Type M omitted for null targets?

Type S is the probability of an incorrect sign among significant estimates; Type M is their mean
absolute exaggeration ratio, `abs(estimate) / abs(truth)`.
[Gelman and Carlin (2014)](https://www.stat.columbia.edu/~gelman/research/published/retropower_final.pdf)
explain these complementary measures of estimation performance. At zero, a true effect has no direction
and its exaggeration ratio divides by zero.

cjsimPWR sets both empirical and analytic Type S/M to `NA` for exact and calibrated null targets. This
also avoids interpreting an arbitrary tiny calibration residual as a substantive directional effect,
with an enormous Type M ratio. `analytic_power` is likewise `NA` for null targets. For non-null targets,
Type S/M use the numerical generated-effect reference; their uncertainty remains conditional on it.
They remain undefined if that numerical truth is zero, and empirical Type S/M are also unavailable
when no valid run is significant.

## Examples and practical use

Include a zero effect to evaluate a genuine null while estimating power for other effects:

```r
library(cjsimPWR)

result <- power_sim(
  levels = c(2, 2), true_amce = list(0.05, 0),
  units = 250, n_tasks = 3, sigma = 0.05,
  sim_runs = 1000, seed = 42
)
result
result$performance[, c("attribute", "requested_amce", "null_status",
                        "power", "type_1_error", "type_1_error_mcse", "n_valid")]
```

Here the second attribute's population AMCE is exactly zero even with heterogeneity. To study the
first attribute's Type I error, rerun with its requested AMCE set to zero, retaining the design,
heterogeneity and inference settings. That changes the population scenario; its null rejection rate
cannot be inferred from nonsignificant runs in the original nonzero-effect simulation. An all-zero
scenario is useful, but it does not establish Type I error for every configuration of other effects.

An all-zero run is still a quick first screen, because it reports a rate for every AMCE and group
difference at once. One check used a 2-level and a 5-level attribute, a group of 200 respondents and a
group of 40, 8 tasks per respondent, `sigma = 0.10` and 2,000 simulated experiments per scenario, so each
rate has a Monte Carlo SE of about 0.5 percentage points. Averaged over five null effects each, the
all-zero rates were:

| Analysis | Group of 200 | Group of 40 | Differences between the groups |
| --- | ---: | ---: | ---: |
| CR1 with normal critical values (default) | 4.9% | 6.2% | 6.0% |
| CR1 with `inference = "t"` | 4.8% | 6.1% | 5.8% |

The null effects that the planned design also contained, with the other effects left at their nonzero
targets, had rates within 0.5 percentage points of their all-zero rates, which is within simulation
error. In this design the rate depended on the group, not on the attribute's number of levels or on the
sizes of the other effects: the small group and the group differences were about one percentage point
above 5%, which a null check in the large group alone would have missed.

For a subgroup difference that requests equality of nonzero effects:

```r
groups <- power_sim(
  levels = c(2, 2), groups = c("A", "B"),
  true_amce = list(A = list(0.10, 0.05), B = list(0.02, 0.05)),
  units = c(A = 250, B = 250), n_tasks = 3, sigma = 0.05,
  sim_runs = 1000, seed = 43
)
null_difference <- subset(groups$performance,
                          type == "difference" & attribute == "var_2")
null_difference[, c("requested_amce", "true_amce", "null_status",
                    "reference_mcse", "reference_half_width", "target_error_bound",
                    "type_1_error", "type_1_error_mcse", "null_size_sensitivity",
                    "bias", "bias_mcse", "coverage",
                    "coverage_reference_lower", "coverage_reference_upper")]
groups$diagnostics[[1]]$verification
groups$diagnostics[[1]]$reference
```

These populations differ on the first attribute, so the equal second-attribute requests are calibrated
separately and their difference is a `calibrated` null. Inspect the residual contrast (`true_amce`) and
its bound (`target_error_bound`) alongside the approximate Type I error. The two diagnostic calculations
show whether the independent recheck confirmed the calibration: `$reference$accepted = FALSE` without a
warning means the recheck was inconclusive, which is recorded quietly, while a warning means an interval
lay wholly outside its tolerance band. Both outcomes are explained under
[how the reference is calculated](#how-the-reference-is-calculated). For more precise reference values,
choose larger budgets before the run, as described under
[options for greater reference precision](#options-for-greater-reference-precision). A tighter tolerance,
such as `calibration_control = list(tolerance = 0.0005)`, demands more precision and can take longer or
fail if that precision cannot be established.

Increasing only `sim_runs` improves experiment-rate precision. Increasing `cores` changes runtime, not
statistical precision. Neither reduces the reference error for a fixed prepared model. A reused `$model`
also retains its original reference; it does not silently recalibrate.

For users summarising their own experiment estimates, the optional arguments are:

```r
# estimates and standard_errors are vectors from repeated experiments;
# the supplied reference below must be independent of those experiments.
summarise_runs(
  estimates, standard_errors, truth = 0.00006,
  null = TRUE, reference_mcse = 0.00004, reference_half_width = 0.00029
)
```

By default, `null = truth == 0`, `reference_mcse = 0` and `reference_half_width = 0`.
`power_sim()` supplies these automatically. Standalone `summarise_runs()` has no requested-target table,
so it does not construct `null_status`, `target_error_bound` or the normal sensitivity warning; users
supplying an approximate null must retain and explain those reference details themselves.

## How the reference is calculated

This and the following sections give the technical details behind the rules above. The logit model
is fitted once for each group in a `power_sim()` call, in three stages:

1. **Calibration:** solve for utility parameters using a training reference.
2. **Verification:** use independent draws to assess whether the generated AMCEs and heterogeneity
   meet the requested precision. Verification can increase its batch count, and failed verification
   can trigger another calibration attempt. If no attempt passes, the call stops with an error.
3. **Final reference:** after acceptance, use a fresh random substream and a budget fixed before its
   draws. By default this uses the accepted verification's integration settings and batch count;
   optional precision planning chooses the batch count as described below. This result supplies
   `true_amce` and its uncertainty. It is never retried, enlarged or used to recalibrate based on its
   observed result.

Separating the final reference avoids selecting the reported truth because it happened to satisfy the
acceptance rule. Conditional on the chosen model and reference budget, the final draws are independent
of those used to choose them. Exact integration requires no fresh draws. A structural-zero mean is
known exactly, although Monte Carlo draws may still be needed to calibrate or measure heterogeneity.
The final reference uses a fresh substream, separating its draws from the experiment streams even if
the experiment seed equals the calibration seed. Calibration draws themselves can overlap experiment
draws when those seed values coincide. Calibration/reference calculations preserve the caller's RNG state.

Verification uses the default AMCE tolerance 0.001. With multiple groups, each group's AMCE is checked
within half the requested tolerance, so the difference is controlled within the full tolerance. AMCE SDs
are checked within `min(0.005, 0.05 * sigma)` when calibrating a positive `sigma`.

Reference MCSEs come from independent batch means. Interval half-widths use t critical values adjusted
for simultaneous comparisons: effects, groups and, when applicable, SDs. Verification additionally
accounts for its possible looks and calibration attempts; the fresh final reference has one fixed look.
The resulting 99% Monte Carlo confidence level is **nominal**, relying on the batch-based approximations.
It is separate from the experiment's significance level `alpha` and is not a deterministic error limit.

Inspect the two calculations in `result$diagnostics[[g]]$verification` and
`result$diagnostics[[g]]$reference`. For a targeted quantity, let `d = abs(estimate - target)`,
`h = reference half-width` and `t = its tolerance`. There are three outcomes:

| Final-reference result | Condition | Diagnostic and behaviour |
| --- | --- | --- |
| Confirmed | `d + h <= t` for every targeted quantity. | `accepted = TRUE`, `contradicted = FALSE`. |
| Inconclusive | At least one check is not confirmed, but no interval is wholly outside its tolerance band. | `accepted = FALSE`, `contradicted = FALSE`; no warning. |
| Contradicted | `d - h > t` for at least one targeted quantity. | `accepted = FALSE`, `contradicted = TRUE`; warn. |

AMCEs and targeted AMCE SDs use their respective tolerances. Touching the tolerance boundary does not
count as a contradiction. The model and numerical reference are retained for all three outcomes.
Failure to reconfirm is common near a precision boundary: verification can stop as soon as containment
is established, while an independent reference at the same budget need not establish it again. Keeping
inconclusive checks quiet does not turn them into confirmation. Repeatedly drawing final references until
one passes would reintroduce selection.

For example, a fresh SD check can give `d + h = 0.00333` and `d - h = 0.00078` against a tolerance of
0.0025. Its interval overlaps the permitted band, so it is inconclusive rather than contradictory.
An exact population null remains exact if the unresolved quantity is only its heterogeneity SD.

If all generated parameters lie within their tolerance bands and the simultaneous final-reference
intervals achieve 99% coverage, a false contradiction has probability at most 1% for that complete
check. This is a **nominal** statement under the batch-t and SD approximations, not a strict finite-sample
guarantee or protection across repeated calls. A contradiction is evidence to investigate the calibration,
not proof that the model is wrong. This warning is separate from the approximate-null sensitivity warning
described below.

### Options for greater reference precision

An inconclusive reference does not automatically require more computation. If a report needs more
precise reference values, choose larger budgets before running it. For the grouped example below,
this existing per-design recipe confirmed both groups in a diagnostic run:

```r
calibration_control = list(
  calibration_draws = 8192,
  verification_draws = 4096,
  verification_batches = 64
)
```

Larger calibration samples can improve the fitted model; larger verification/reference samples improve
numerical precision. This recipe is not a universal setting. With sampled profiles, increasing
`calibration_pairs` and `verification_pairs` may also be necessary. `sim_runs` controls repetitions of
the experiment and does not reduce calibration/reference uncertainty.

An alternative experimental option allocates work using an explicit margin and precision target:

```r
calibration_control = list(reference_margin = 0.5)
# Optional final-reference cap: max_reference_batches = 512 (the default, per group).
```

The default `reference_margin = 0` disables this option and preserves previous results and budgets.
For a positive margin `m < 1`, let `t_j` be each quantity's existing tolerance: the per-group AMCE
tolerance, or `min(0.005, 0.05 * sigma)` for a targeted SD. The rules are:

1. **Reserve headroom.** Verification accepts only when every estimated target discrepancy plus its
   uncertainty margin is at most `(1 - m) * t_j`. Failure uses the existing verification looks and
   calibration retries; the latter enlarge training samples. If no attempt passes the stricter
   verification, the call stops with an explicit reserved-margin error; existing solver limits also
   apply. A positive margin can therefore cost more or fail even when the default settings succeed.
   It does not change the final tolerances.
2. **Plan precision.** Recover each batch variance from verification's MCSE and batch count, and
   multiply that variance by two as a conservative planning allowance. Choose the smallest allowed
   final batch count whose predicted half-width is at most `m * t_j / 2` for every targeted quantity.
   Use the final-reference t critical value, including its degrees of freedom and simultaneous
   comparisons across groups and effects. Retain the accepted integration settings within each batch,
   so extra independent batches address both coefficient and profile-integration uncertainty.
3. **Fix and cap the budget.** The minimum is `verification_batches`; the maximum is
   `max_reference_batches` (512 by default, and at least the minimum when this option is enabled).
   If the predicted precision requires more than the cap, use the cap and record that limitation.
   Draw the final reference once, retain its values, and assess confirmation against the original
   tolerances. Neither a cap hit nor a missed width target triggers redrawing or a warning by itself.

With `m = 0.5`, verification uses half of each tolerance and planning targets final half-widths of
one quarter of it. The factor of two inflates **variance**, not the half-width. This is a heuristic
allowance, not a variance confidence bound. The accepted verification may have unusually small
estimated variances, the realised final widths are random, and the SD intervals use approximations.
Formal fixed-width methods require additional assumptions and treatment of variance uncertainty;
see Hickernell et al. (2013).

If the verification and final intervals contain the generated values **and the realised final widths
meet the target**, then `final gap + final half-width <= (1 - m) * t_j + 2 * final half-width <= t_j`.
Predicted widths do not ensure this event, especially when the cap binds. The option therefore makes
confirmation more likely, by reserving calibration headroom and planning additional computation, but it
does not guarantee it.

Inspect `result$diagnostics[[g]]$reference_plan` when the option is enabled:

| Field | Meaning |
| --- | --- |
| `margin`, `variance_inflation` | The reserved fraction and the planning variance multiplier (two). |
| `batches`, `max_batches` | The fixed final batch count and configured cap. |
| `cap_limited`, `predicted_precision_met` | Whether the cap prevents the predicted widths from meeting their targets, and whether those targets are predicted to be met. |
| `precision_met` | Whether all **actual** final half-widths meet the planning targets. |
| `quantities` | Attributes/levels, quantity type, original tolerance, target/predicted/actual half-widths and individual precision flags. |

Width precision is separate from confirmation: a narrow interval can reveal a target discrepancy,
and an interval wider than the planning target can still lie inside the tolerance band. Continue to
inspect `$reference$accepted` and `$reference$contradicted`. Known exact references need zero batches;
inactive exact-zero attributes need no planning. Shared populations reuse the same reference and plan.
When `latent_sigma` is supplied instead of `sigma`, only AMCEs have targets; the implied SDs do not.
The deprecated odds model has no requested AMCE/SD targets and rejects a positive `reference_margin`.

## Rejection rates and their simulation precision

For valid experiment `r`, with estimate `beta_r`, standard error `se_r` and critical value `c_r`, the
rejection indicator is `I(abs(beta_r / se_r) > c_r)`. Normal inference uses `qnorm(1 - alpha/2)`;
t inference uses the appropriate per-run degrees of freedom. If `n = n_valid`, the implementation is

```text
rejection rate = number of significant valid runs / n
rate MCSE     = sqrt(rejection rate * (1 - rejection rate) / n)
```

The same rate is stored as power or Type I error according to the target classification. This MCSE
measures the precision of the observed rejection rate under the **generated** model. It does not quantify
the difference between approximate-null rejection and exact-null Type I error. The binomial MCSE and
its role in simulation reporting are described by
[Morris, White and Crowther (2019)](https://doi.org/10.1002/sim.8086).

At a rejection probability of 0.05, the expected MCSE is about 0.0069 with 1,000 valid runs and 0.00154
with 20,000: about 0.69 and 0.154 percentage points. These are standard errors, not 95% margins. The
plug-in MCSE is zero if every valid run rejects or none does; that does not establish a population
probability of exactly one or zero.

Runs with unavailable estimates, invalid standard errors or unavailable inference degrees of freedom
are excluded and counted in `n_failed`. They are never counted as nonsignificant. With no valid runs,
the rate is `NA`. Because summaries condition on valid inference, a substantial failure rate can make
them unrepresentative of the planned procedure; inspect `$failures` as well as the percentages.

## The bound for a calibrated null

For any row with a requested AMCE, the package records

```text
b = target_error_bound
  = abs(true_amce - requested_amce) + reference_half_width
```

If the reference interval contains the generated effect, the triangle inequality gives
`abs(theta - tau) <= b`. For a null target `tau = 0`, this becomes `abs(theta) <= b`. Thus `b` is a
confidence bound on the remaining departure from the requested null. It is not the effect's standard
error in a simulated experiment, and it is not an unconditional guarantee that the generated effect
is smaller than `b`.

For independently calibrated groups A and B, the contrast reference uses

```text
theta_ref_difference = theta_ref_B - theta_ref_A
reference_mcse_difference = sqrt(reference_mcse_A^2 + reference_mcse_B^2)
reference_half_width_difference = reference_half_width_A + reference_half_width_B
```

Adding interval half-widths preserves the contrast bound on the simultaneous reference confidence
event. When the population and reference are shared, their errors are perfectly correlated and cancel;
the implementation sets the contrast reference error to zero instead. Odds inputs have no requested
AMCE, so `target_error_bound` is `NA` for those rows.

### How large is the resulting departure from an exact-null rejection rate?

Consider an illustrative contrast with `true_amce = 0.00006` and `reference_half_width = 0.00029`.
Then `b = 0.00035`. Its reported rejection rate is formally rejection under the generated contrast,
which may be nonzero. Calling it approximate Type I error is informative only if this departure is
small enough for the intended analysis.

To assess scale, the package uses a benchmark with an unbiased normal estimator and a fixed known
standard error `s`. Let `z = qnorm(1 - alpha/2)` and let `Phi` and `phi` denote the standard normal
distribution and density. Under that benchmark the excess rejection probability at effect magnitude
`b` is

```text
Delta(b/s) = Phi(-z - b/s) + Phi(b/s - z) - alpha
          = z * phi(z) * (b/s)^2 + O((b/s)^4), for small b/s.
```

The first-order term cancels because the test is two-sided. At `alpha = 0.05`, `z * phi(z)` is about
0.11455. With `b = 0.00035` and `s = 0.02`, the exact normal expression gives about 0.0000351 excess
rejection probability, or 0.00351 percentage points. This is small relative to a null-rate MCSE of
0.0069 at 1,000 runs or 0.00154 at 20,000 runs.

`null_size_sensitivity` evaluates the full normal expression with `s = emp_se`, the empirical SD of
the estimates. It is zero for exact nulls and `NA` for non-null targets or unavailable bounds/empirical
SEs. For a calibrated null, a warning appears when

```text
null_size_sensitivity > 0.1 * sqrt(alpha * (1 - alpha) / n_valid).
```

The one-tenth threshold is a package diagnostic choice, not a literature-established acceptance rule.
It uses the nominal null MCSE, avoiding a zero threshold merely because a small simulation observed no
rejections. More runs can make a previously negligible calibration discrepancy relevant to the new
reporting precision.

This benchmark is **not a bound on size distortion for the fitted conjoint analysis**. It assumes an
unbiased normal estimate with known SE; cluster-robust SEs are estimated and small-sample distributions
may be biased or non-normal. Even an exact null with sensitivity zero can have inflated Type I error
because of the analysis method. Absence of this warning therefore does not validate the inference.

### Coverage and sensitivity to the reference

`coverage` is the share of experiment confidence intervals containing `true_amce`.
`coverage_mcse = sqrt(coverage * (1 - coverage) / n_valid)` remains conditional on that numerical
reference. No reference variance is simply added to this binomial formula: interval membership is a
nonlinear, discontinuous function of the truth.

Instead, the package supplies conservative sensitivity bounds over the reference interval
`J = [true_amce - reference_half_width, true_amce + reference_half_width]`:

| Column | Calculation |
| --- | --- |
| `coverage_reference_lower` | Fraction of experiment confidence intervals containing all of `J`. |
| `coverage_reference_upper` | Fraction of experiment confidence intervals intersecting `J`. |

For any single candidate truth inside `J`, its empirical coverage is between those values. The bounds
can be conservative because different experiment intervals may intersect different parts of `J`.
They coincide with `coverage` when the reference half-width is zero. They are not confidence intervals
for population coverage and do not replace the simulation MCSE.

For an exact zero evaluated with the same two-sided test and interval, coverage is `1 - type_1_error`.
For a calibrated null, coverage is evaluated around the numerical generated-effect reference while
rejection still tests zero, so that identity need not hold. Other reported MCSEs, including Type S/M for
non-null targets, remain conditional on the numerical reference. If reference uncertainty matters,
increase reference precision rather than interpreting those conditional MCSEs as total uncertainty.

## Comparison with Schuessler and Freitag (2020)

Schuessler and Freitag's [cjpowR implementation](https://github.com/m-freitag/cjpowR/blob/5852d88338235abc28919b2941ea56fe0532d48d/R/amce.R)
computes rejection probability from a normal approximation and an assumed SE. Setting the AMCE to zero
in its two-tail expression, with a sample size supplied, yields the chosen `alpha` in the column labelled
`power`. At `alpha = 0.05`, the two tails contribute 2.5% each. This is the theoretical Type I error under
that normal model, not a measured false-positive rate. Increasing the sample size cannot change this
zero-effect value. Its simulation option for Type M draws normal estimates and serves a different
purpose from generating and analysing full conjoint datasets.

cjsimPWR estimates the rejection rate after generating complete experiments and applying the requested
inference method. This can reveal departures from nominal size that a formula assumes away. Agreement
with a closed-form power estimate therefore does not itself validate Type I error. For example, the
formula gives 5% at a zero effect even if the fitted analysis would reject that null too often with
few respondents. A simulation can reveal that discrepancy, subject to its own Monte Carlo uncertainty
and the population assumptions used to generate the data. The
[closed-form comparison](closed_form.md#type-i-error-equals-the-significance-level-3) gives the full
expression, practical use cases and a step-by-step explanation of the comparison.

## References

- Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing Type S (sign) and Type M
  (magnitude) errors. *Perspectives on Psychological Science*, 9(6), 641–651.
  [Author-hosted paper](https://www.stat.columbia.edu/~gelman/research/published/retropower_final.pdf).
- Hickernell, F. J., Jiang, L., Liu, Y., & Owen, A. (2013). Guaranteed conservative fixed width
  confidence intervals via Monte Carlo sampling. In *Monte Carlo and Quasi-Monte Carlo Methods 2012*
  (pp. 105–128). Springer. [Paper](https://doi.org/10.1007/978-3-642-41095-6_5);
  [author manuscript](https://arxiv.org/abs/1208.4318).
- Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation studies to evaluate statistical
  methods. *Statistics in Medicine*, 38(11), 2074–2102. [Paper](https://doi.org/10.1002/sim.8086).
- Naimi, A. I., Benkeser, D., & Rudolph, J. E. (2025). Computing true parameter values in simulation
  studies using Monte Carlo integration. *Epidemiology*, 36(5), 690–693.
  [Paper](https://doi.org/10.1097/EDE.0000000000001873).
- Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for cluster-robust variance estimation
  and hypothesis testing in fixed effects models. *Journal of Business & Economic Statistics*, 36(4),
  672–683. [Paper](https://doi.org/10.1080/07350015.2016.1247004).
- Schuessler, J., & Freitag, M. (2020). Power analysis for conjoint experiments. SocArXiv.
  [Working paper](https://doi.org/10.31235/osf.io/9yuhp);
  [authors' implementation](https://github.com/m-freitag/cjpowR).

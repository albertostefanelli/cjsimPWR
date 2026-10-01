# Calibration and reference precision

When you ask `power_sim()` or `simulate_experiment()` for an AMCE, for example that being a woman raises
a candidate's chance of being chosen by 5 percentage points (an AMCE of 0.05), the package must create
simulated respondents who actually produce that effect. This is less direct than it sounds. Simulated
respondents do not choose by AMCEs: each attribute level makes a candidate more or less attractive to
them, and they tend to choose the more attractive of the two candidates on screen. How much extra
attractiveness changes choices depends on the rest of the screen: it moves more choices when the two
candidates are otherwise evenly matched than when one is far more attractive anyway, much as a small
swing decides a close race but not a landslide. So no simple formula turns a requested AMCE into the
right amount of attractiveness. *Calibration* is the search for it: the package tries settings, measures
the AMCEs they produce and adjusts them until the measured AMCEs match your request. It then measures
the accepted settings one final time, and this final measurement is the answer key against which the
simulated experiments are scored.

This guide explains that process, how to read its checks and how to make its measurements more
precise; you mainly need it when a check warns or stops with an error. See the
[simulation model](simulation_model.md) for the choice model itself and
[Type I error and reference uncertainty](type_1_error.md) for reading power and Type I error output.

Jump to: [targets and numerical truth](#1-targets-and-numerical-truth),
[calibration, verification and final reference](#2-calibration-verification-and-final-reference),
[reading diagnostics](#3-reading-diagnostics), [choosing precision](#4-choosing-precision),
[bounds and sensitivity](#5-bounds-and-sensitivity),
[reproducibility and legacy behaviour](#6-reproducibility-and-legacy-behaviour).

## 1. Targets and numerical truth

The package keeps three versions of each effect apart:

| Quantity | Meaning | Where to find it |
| --- | --- | --- |
| Requested effect, `tau` | The AMCE you asked for, or the difference between two groups' requests. | `requested_amce` |
| Generated effect, `theta` | The AMCE the simulated respondents actually produce after calibration. | Fixed by the calibrated model, but usually known only through the measurement below. |
| Reference estimate, `theta_ref` | The package's measurement of the generated effect: the answer key. | `true_amce`, with its precision in `reference_mcse` and `reference_half_width` |

These columns are in the `truth` table: `attr(data, "dgp")$truth` for `simulate_experiment()` output,
or `$truth` for `power_sim()` output. The name `true_amce` has two uses. As an input argument of
`simulate_experiment()` and `power_sim()`, it holds the AMCEs you request; in the `truth` table, your
request is reported as `requested_amce`, and the `true_amce` column always holds the measurement.

`reference_half_width` is the measurement's margin of error: as with a poll, the generated effect lies
within this distance of the measurement, at a nominal 99% confidence. `reference_mcse` is its standard
error: how much the measurement would vary if it were repeated with new random samples.

Two things can separate your request from the answer key:

- **Calibration error** (`theta` differs from `tau`): the search stops once the generated effect is
  within a small allowed distance of your request, the `tolerance` (0.001, or 0.1 percentage points, by
  default), not necessarily exactly on it.
- **Measurement error** (`theta_ref` differs from `theta`): when the package measures by random
  sampling, the measurement has a margin of error. Counting every possible pair of candidates, where
  the design allows it, removes this error unless respondents differ in their preferences
  (`sigma > 0`) and must be sampled too; it never removes the calibration error. See
  [section 2](#2-calibration-verification-and-final-reference).

These gaps are why simulated experiments are scored against the answer key rather than against your
request. Suppose Democratic and Republican respondents have the same 5-point effect of candidate gender
but different effects of candidate party, so the two groups are calibrated separately. Their generated
gender effects can come out slightly apart, and the measured difference between the groups might be
0.00006 instead of zero. Keep that residual, and its `reference_mcse`, when judging bias and coverage;
do not replace them with zero. This follows the general advice to evaluate a simulation against the
effect it actually generated and to report how precisely that effect was measured
([Naimi, Benkeser and Rudolph, 2025](https://doi.org/10.1097/EDE.0000000000001873)). The gaps are usually
tiny next to the standard error of an AMCE in a real study (typically 0.01 to 0.05), so they rarely
change a conclusion; the package reports them for transparency and reproducibility.

## 2. Calibration, verification and final reference

For the default logit model, calibration runs once for each group of respondents, in three steps. (With
`dgp = "linear"`, your requested AMCEs are used directly as the model's settings, so none of these
steps is needed and `true_amce` equals your request exactly; that model requires `sigma = 0`.)

1. **Calibrate.** The package searches for the attractiveness settings (utility coefficients) that
   reproduce your requested AMCEs and, with `sigma > 0`, for how much respondents differ, so that the
   spread (standard deviation, SD) of their individual AMCEs matches `sigma`. Each time it tries a
   setting, it measures the AMCEs that setting produces. Because an AMCE is an average over the other
   attributes and the competing candidate, measuring it means averaging over every pair of candidates
   a respondent could be shown.

   For example, three attributes with 2, 3 and 5 levels give 2 × 3 × 5 = 30 possible candidate
   profiles. Either side of the screen can show any of them, so there are 30 × 30 = 900 possible pairs:
   few enough to check every pair, like counting every ballot. The measurement is then exact (*exact
   enumeration*). But each added attribute multiplies the count. A candidate conjoint with gender
   (2 levels), age (4), party (2), political experience (3), religion (4) and immigration stance (3)
   has 576 profiles and 331,776 pairs. When a design has more than `exact_max_pairs` pairs (10,000 by
   default, that is, more than 100 profiles), the package works like a poll instead: it averages over a
   large random sample of pairs (*Monte Carlo integration*), and the measurement has a small margin of
   error. Most realistic designs are in this second case. With `sigma > 0`, the package also averages
   over a random sample of simulated respondents, so there is a margin of error even when every pair
   is counted.
2. **Verify.** Using fresh random samples, independent of those used in the search, the package checks
   that every AMCE is within `tolerance` of your request and, with `sigma > 0`, that every AMCE's SD is
   within `min(0.005, 0.05 * sigma)` of `sigma` (0.0025 when `sigma = 0.05`). The check passes only
   when the measurement *and its whole margin of error* fit inside this allowed band, at a nominal 99%
   confidence ("nominal" because the 99% rests on standard statistical approximations). With groups,
   each group's AMCE must be within half the tolerance, so that the difference between two groups stays
   within the full tolerance. If the check fails, the package takes more samples and, if needed,
   repeats the search with a larger sample; if it still fails after `max_attempts` tries, the call
   stops with an error.
3. **Final reference.** After the check passes, the package measures the accepted model once more, with
   its own stream of random numbers and a sample size fixed in advance (by default, the same as in the
   check that passed). Whatever this measurement shows, it is never repeated, enlarged or used to change
   the model. Otherwise the package could keep measuring until a result looked good, like rerunning a
   poll until it shows the result you want, and the reported answer key would be biased. This final
   measurement supplies `true_amce`, `amce_sd` and their precision in the `truth` table. An exact
   measurement needs no new samples. A requested AMCE of exactly zero is built in by symmetry, so its
   `true_amce` is exactly zero, with no margin of error, even when other effects need sampling.

Groups whose requests are identical for every effect share one calibration and one final measurement,
and so do repeated calls that reuse a prepared `model`. A difference between such populations is
exactly zero, with no measurement error. The final measurement's random numbers are always separate
from those that simulate the experiments, even when calibration and experiments are given the same seed
(see [reproducibility](#6-reproducibility-and-legacy-behaviour)).

## 3. Reading diagnostics

Each group's checks are stored in its diagnostics: `attr(data, "dgp")$diagnostics[[g]]` for
`simulate_experiment()` output, or `result$diagnostics[[g]]` for `power_sim()` output, where `g` is the
group's position (`1` without groups). `$verification` holds the step 2 check that let calibration
finish; `$reference` holds the final measurement, checked against the same bands. Its `method` field
says how the effects were measured, for example `"exact"` (every pair counted), `"exact profiles /
Monte Carlo coefficients"` (every pair counted, respondents sampled) or `"Monte Carlo"` (pairs sampled).

For each targeted quantity, let `d` be the distance between the final measurement and its target (your
requested AMCE, or `sigma` for an SD), `h` the measurement's margin of error (`reference_half_width`, or
`sd_half_width` for an SD) and `t` its allowed band (`tolerance` for an AMCE, halved for each group when
you simulate groups, or `min(0.005, 0.05 * sigma)` for an SD):

| Final-reference result | Meaning and condition | `accepted` / `contradicted` | What to do |
| --- | --- | --- | --- |
| Confirmed | Every measurement, with its whole margin of error, is inside its band: `d + h <= t` for every targeted quantity. | `TRUE` / `FALSE` | Nothing. |
| Inconclusive | Some margin of error crosses the edge of its band, but none lies wholly outside it. | `FALSE` / `FALSE`, no warning | Nothing needed; increase precision ([section 4](#4-choosing-precision)) if your report needs a confirmed check. |
| Contradicted | At least one margin of error lies wholly outside its band: `d - h > t` for at least one targeted quantity. | `FALSE` / `TRUE`, with a warning | Investigate the calibration; the model and its measurement are kept. |

For example, with a requested AMCE of 0.05, the default tolerance of 0.001 and a margin of error of
0.0003, a measurement of 0.0504 is confirmed (0.0004 + 0.0003 ≤ 0.001), 0.0508 is inconclusive
(0.0008 + 0.0003 > 0.001, but 0.0008 - 0.0003 ≤ 0.001) and 0.0515 is contradicted
(0.0015 - 0.0003 > 0.001).

A margin of error that just touches the edge of its band is inconclusive, not contradicted, and an
inconclusive result is recorded quietly: on its own it does not signal a problem. If the generated
population really is within its bands, the chance of a false contradiction warning in a single call is
at most about 1 in 100. That figure is nominal: it rests on statistical approximations (the batch-t and
SD approximations), is not an exact guarantee and applies to each call separately, so over many calls
an occasional false warning is expected. A contradiction is therefore a reason to investigate, not proof
that the model is wrong.

This warning is separate from `power_sim()`'s internal sensitivity warning for approximate null
effects, whose definition and threshold are in the `power_sim()` help (section "Interpreting results").

## 4. Choosing precision

If a check is inconclusive, or you want smaller margins of error, give the package larger samples. The
standard recipe enlarges the calibration and verification samples together. It does not change the
allowed bands, so it makes the measurements more precise rather than making the check easier to pass:

```r
library(cjsimPWR)

design <- conjoint_design(c(2, 3, 5))
amce   <- list(0.05, c(-0.05, 0.05), c(-0.03, -0.05, -0.05, 0.05))
precise <- simulate_experiment(design, amce, units = 500, n_tasks = 5, sigma = 0.05,
  calibration_control = list(
    seed = 2024,               # calibration's own seed, independent of experiment sampling
    calibration_draws = 8192,  # larger search sample can bring the model closer to the request
    verification_draws = 4096, # larger check and final samples give smaller margins of error
    verification_batches = 64
  ))
attr(precise, "dgp")$truth
```

The settings form two sets. `calibration_pairs` and `calibration_draws` set the size of the sample the
search works with (candidate pairs and simulated respondents); larger values can bring the fitted model
closer to your request. `verification_pairs`, `verification_draws` and `verification_batches` set the
samples used by the check and the final measurement. The package takes these samples in independent
batches and judges its own precision by how much the batches disagree, so more or larger batches give
smaller margins of error.

This recipe is not a universal setting. The example design has only 900 pairs, all of which are
counted, so the pair settings have no effect there; the recipe enlarges the respondent samples and the
number of batches instead. When pairs are sampled, increasing `calibration_pairs` and
`verification_pairs` may also be necessary. Larger samples take longer: the recipe above runs several
times longer than the defaults, and calibration can take many minutes when `sigma > 0` and pairs are
sampled. `sim_runs`, the number of simulated experiments, does not help here: it makes power and Type I
error estimates more precise, not the answer key of a given model.

An experimental alternative, `reference_margin`, keeps some headroom during the check and then plans
the size of the final measurement from the check's results, instead of you enlarging every sample by
hand. It is off by default. Its full rules (allowed values, the `max_reference_batches` cap, effects on
run time and failures, and the `reference_plan` diagnostic fields) are in the Calibration section of
the `simulate_experiment()` help. This kind of planning is motivated by methods for fixed-width Monte
Carlo confidence intervals ([Hickernell et al., 2013](https://doi.org/10.1007/978-3-642-41095-6_5)),
which rest on additional assumptions about how much the samples vary.

## 5. Bounds and sensitivity

How far can the generated effect be from your request? For any row with a requested AMCE, the `truth`
table reports

```text
target_error_bound = abs(true_amce - requested_amce) + reference_half_width
```

that is, the gap between the measurement and your request plus the measurement's margin of error. If
the generated effect lies within the margin of error, as it does at a nominal 99% confidence, it is at
most `target_error_bound` away from your request: `abs(theta - tau) <= target_error_bound`. For
example, a request of 0.05, a measurement of 0.0498 and a margin of error of 0.0001 give a bound of
0.0003: the generated effect is within 0.03 percentage points of your request. This is a confidence
statement, not a guarantee, and it is not the effect's standard error in a simulated experiment. The
deprecated odds model has no requested AMCE, so its bound is `NA`.

For two separately calibrated groups A and B, the measurement of their difference and its precision
combine as:

```text
theta_ref_difference            = theta_ref_B - theta_ref_A
reference_mcse_difference       = sqrt(reference_mcse_A^2 + reference_mcse_B^2)
reference_half_width_difference = reference_half_width_A + reference_half_width_B
```

The measurements subtract, and their standard errors combine as independent errors do. The margins of
error add, which keeps the bound valid for the difference whenever both groups' margins hold at the
same time. When the two groups share one population (section 2), their measurement errors are
identical and cancel, so the difference has no measurement error at all.

See [Type I error and reference uncertainty](type_1_error.md#interpreting-reference-sensitivity) for how
to interpret Type I error and its sensitivity warning, and the `summarise_runs()` help for the
definitions of the `coverage_reference_lower` and `coverage_reference_upper` bounds.

## 6. Reproducibility and legacy behaviour

A seed is the starting point of a sequence of random numbers: the same seed gives the same results.
Calibration has its own seed, `calibration_control$seed`, separate from the seed of the simulated
experiments, and it leaves your R session's random numbers as they were. Changing the experiment seed
therefore changes the simulated experiments but not the calibrated population. If you give both the
same seed value, some of their random draws can coincide, because both start from the same point; the
final measurement always uses a separate stream regardless, keeping its draws apart from the
experiments'. See the `power_sim()` help (section "Reproducibility") for the full guarantee across
worker counts and repeated calls.

The deprecated odds model (`dgp = "odds"`) takes fixed scores rather than AMCE or SD targets, so there
is nothing to search for. Its check only confirms that the measurement is precise enough, doubling the
pair and respondent samples on each retry, up to `max_attempts`. Its `accepted` flag therefore reflects
precision alone, and `contradicted` is always `FALSE`, since there is no target to contradict. How to
migrate from odds-model arguments is described in NEWS, not repeated here.

## References

- Hickernell, F. J., Jiang, L., Liu, Y., & Owen, A. (2013). Guaranteed conservative fixed width
  confidence intervals via Monte Carlo sampling. In *Monte Carlo and Quasi-Monte Carlo Methods 2012*
  (pp. 105–128). Springer. [Paper](https://doi.org/10.1007/978-3-642-41095-6_5);
  [author manuscript](https://arxiv.org/abs/1208.4318).
- Naimi, A. I., Benkeser, D., & Rudolph, J. E. (2025). Computing true parameter values in simulation
  studies using Monte Carlo integration. *Epidemiology*, 36(5), 690–693.
  [Paper](https://doi.org/10.1097/EDE.0000000000001873).

# Type I error, null targets and reference uncertainty

cjsimPWR reports how often the planned analysis rejects the hypothesis that an effect is zero. When the
simulated effect is zero, that rate is the Type I error (false-positive) rate; otherwise it is power.
This guide explains how to read these results and check them for a planned design.

**In brief**

- Include a zero AMCE in the design to see the Type I error of the planned analysis next to power. At
  `alpha = 0.05` it should be close to 5%, allowing for its Monte Carlo standard error.
- Zero targets report Type I error; nonzero targets report power. A nonsignificant estimate never
  turns power into Type I error.
- Before comparing designs or inference methods, inspect each rate's Monte Carlo standard error and the
  failed-run counts in `$failures`, and compare power against null rejection rates — following
  [Morris, White and Crowther (2019)](https://doi.org/10.1002/sim.8086).

Jump to: [reading the output](#reading-the-output), [checking an effect](#checking-an-effect),
[a subgroup example](#a-subgroup-example),
[Monte Carlo precision and failed runs](#monte-carlo-precision-and-failed-runs),
[interpreting reference sensitivity](#interpreting-reference-sensitivity), or
[the closed-form comparison](#comparison-with-schuessler-and-freitag-2020).

## Reading the output

`power_sim()` classifies a row as a null target when `requested_amce == 0` — using the requested **contrast** for a difference row. Null targets populate `type_1_error`; non-null targets populate `power`. The default printout includes Type I error with its MCSE when available and
omits entirely unavailable measure columns.

An individual zero AMCE is exact, as are zero differences for shared populations and levels that are
zero in both groups. Equal nonzero requests in otherwise different populations can leave a small
residual contrast after separate calibration, so their reported Type I error is approximate. Bias and
coverage still use the generated-effect reference `true_amce`.

`true_amce` is the package's numerical reference for the AMCE actually generated in the simulated
population, not necessarily identical to the requested value. Bias and coverage are measured against it,
and its own uncertainty (`reference_mcse`, `reference_half_width`) is carried into `bias_mcse` and the
coverage-sensitivity bounds described below. See the
[calibration guide](calibration.md#1-targets-and-numerical-truth) for the full requested/generated/reference
distinction.

## Checking an effect

To check a particular effect, run the simulation twice: once with its expected effect to measure power,
and once with its target set to zero to measure Type I error, keeping every other setting — other
effects' targets, groups, sample sizes, tasks, heterogeneity and inference — unchanged. Check each
subgroup and group difference, not only the largest group: this is especially useful when respondents are
few but complete many tasks, when a subgroup is small despite a large overall sample, or when comparing
inference methods, since small-sample clustered inference can reject too often (see
[Pustejovsky and Tipton 2018](https://doi.org/10.1080/07350015.2016.1247004) and the
[clustering guide](clustering.md)).

```r
library(cjsimPWR)

# Education AMCE 0.05, experience AMCE 0.10: measure power for both
power_check <- power_sim(
  levels = c(2, 2), true_amce = list(0.05, 0.10),
  units = 250, n_tasks = 3, sim_runs = 1000, seed = 42
)

# Set education's target to zero to measure its Type I error; experience and
# every other setting stay unchanged
type1_check <- power_sim(
  levels = c(2, 2), true_amce = list(0, 0.10),
  units = 250, n_tasks = 3, sim_runs = 1000, seed = 42
)
type1_check$performance[, c("attribute", "requested_amce",
                            "type_1_error", "type_1_error_mcse", "n_valid")]
```

A single run with every target set to zero is a quick first screen: it reports a rate for every effect at
once. But the rate depends on how many respondents inform each estimate, so an all-zero scenario does not
establish Type I error for every configuration of the other effects.

Type S is the probability of an incorrect sign among significant estimates, and Type M their mean
exaggeration ratio ([Gelman and Carlin 2014](https://www.stat.columbia.edu/~gelman/research/published/retropower_final.pdf)); both are undefined
at a true zero, so cjsimPWR sets them `NA` for all null targets, rather than
reporting an arbitrary calibration residual as a directional effect with an extreme ratio.

## A subgroup example

```r
groups <- power_sim(
  levels = c(2, 2), groups = c("A", "B"),
  true_amce = list(A = list(0.10, 0.05), B = list(0.02, 0.05)),
  units = c(A = 250, B = 250), n_tasks = 3, sigma = 0.05,
  sim_runs = 1000, seed = 43
)
null_difference <- subset(groups$performance,
                          type == "difference" & attribute == "var_2")
null_difference[, c("true_amce", "target_error_bound", "type_1_error", "type_1_error_mcse")]
```

Inspect the residual contrast and its bound alongside the reported rate. A tighter tolerance, such as
`calibration_control = list(tolerance = 0.0005)`,
demands more precision and can take longer or fail if that precision cannot be established. Increasing
only `sim_runs` improves experiment-rate precision — it does not reduce the reference error for a fixed
prepared model; the [calibration guide](calibration.md#4-choosing-precision) covers tightening that
precision.

For summarising your own experiment estimates outside `power_sim()`, see the approximate-null example in
the [`summarise_runs()`] help. Standalone calls construct no target error bound, so you
must supply and interpret any reference details yourself.

## Monte Carlo precision and failed runs

For valid experiment `r`, with estimate `beta_r`, standard error `se_r` and critical value `c_r`, the
rejection indicator is `I(abs(beta_r / se_r) > c_r)`. Normal inference uses `qnorm(1 - alpha/2)`; t
inference uses the per-run degrees of freedom. With `n = n_valid`:

```text
rejection rate = number of significant valid runs / n
rate MCSE     = sqrt(rejection rate * (1 - rejection rate) / n)
```

The same rate is stored as power or Type I error according to the target classification. This MCSE
measures the precision of the observed rejection rate under the **generated** model; it does not quantify
the gap between approximate-null rejection and exact-null Type I error
([Morris, White and Crowther 2019](https://doi.org/10.1002/sim.8086)).

At a rejection probability of 0.05, the expected MCSE is about 0.0069 with 1,000 valid runs and 0.00154
with 20,000 — about 0.69 and 0.154 percentage points. These are standard errors, not 95% margins. The
plug-in MCSE is zero if every valid run rejects or none does; that does not establish a population
probability of exactly one or zero.

Runs with unavailable estimates, invalid standard errors or unavailable inference degrees of freedom are
excluded and counted in `n_failed`, never as nonsignificant. With no valid runs, the rate is `NA`. Because
summaries condition on valid inference, a substantial failure rate can make them unrepresentative of the
planned procedure; inspect `$failures` alongside the percentages.

## Interpreting reference sensitivity

For any row with a requested AMCE, `target_error_bound = abs(true_amce - requested_amce) + reference_half_width`.
If the reference interval contains the generated effect, this bounds the remaining departure from the
requested value at a nominal 99% Monte Carlo confidence — not a strict guarantee, and not the effect's
standard error in a simulated experiment. How this bound combines across separately calibrated groups is in the
[calibration guide](calibration.md#5-bounds-and-sensitivity).

The `power_sim()` help ("Interpreting results") defines the internal normal-benchmark check and its
warning threshold. The check returns no diagnostic column and is unavailable when the reference bound
or empirical SE is non-finite, or the empirical SE is non-positive. An unavailable check does not
establish that calibration error is negligible.

`coverage` is the share of experiment confidence intervals containing `true_amce`; its MCSE remains
conditional on that numerical reference. `coverage_reference_lower` and `coverage_reference_upper` are
conservative *bounds* on coverage over the reference interval — not confidence intervals for coverage —
and coincide with `coverage` when the reference half-width is zero (see `summarise_runs()` help for their
definitions). For an exact zero, coverage equals `1 - type_1_error`; that identity need not hold for an
approximate null contrast, since coverage uses the numerical generated-effect reference while rejection
still tests zero. If reference uncertainty matters, increase reference precision (calibration
guide) rather than treating these conditional MCSEs as total uncertainty.


## Comparison with Schuessler and Freitag (2020)

Their [cjpowR implementation](https://github.com/m-freitag/cjpowR/blob/5852d88338235abc28919b2941ea56fe0532d48d/R/amce.R)
reports the significance level itself as the zero-effect "power" — a theoretical value under a normal
approximation, not a measured false-positive rate. See the
[closed-form comparison](closed_form.md#type-i-error-equals-the-significance-level-3) for the full
expression and a worked comparison against cjsimPWR's simulated rejection rate.

## References

- Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing Type S (sign) and Type M
  (magnitude) errors. *Perspectives on Psychological Science*, 9(6), 641–651.
  [Author-hosted paper](https://www.stat.columbia.edu/~gelman/research/published/retropower_final.pdf).
- Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation studies to evaluate statistical
  methods. *Statistics in Medicine*, 38(11), 2074–2102. [Paper](https://doi.org/10.1002/sim.8086).
- Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for cluster-robust variance estimation
  and hypothesis testing in fixed effects models. *Journal of Business & Economic Statistics*, 36(4),
  672–683. [Paper](https://doi.org/10.1080/07350015.2016.1247004).
- Schuessler, J., & Freitag, M. (2020). Power analysis for conjoint experiments. SocArXiv.
  [Working paper](https://doi.org/10.31235/osf.io/9yuhp);
  [authors' implementation](https://github.com/m-freitag/cjpowR).

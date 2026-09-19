cjsimPWR: Power Analyses for Conjoint Experiments Using Simulation
================

| <img width="30%" src="man/figures/logo.png"> |
|:--------------------------------------------:|

[![R-CMD-check](https://github.com/albertostefanelli/cjsimPWR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/albertostefanelli/cjsimPWR/actions/workflows/R-CMD-check.yaml)

Using the simulation framework of [Stefanelli & Lukac
(2020)](https://doi.org/10.31235/osf.io/spkcy), `cjsimPWR` calculates
the statistical power of forced-choice conjoint experiments. You
describe the design (attributes and levels), the sample (respondents and
tasks per respondent) and the average marginal component effects (AMCEs;
[Hainmueller, Hopkins & Yamamoto,
2014](https://doi.org/10.1093/pan/mpt024)) you expect. The package
simulates the experiment many times, estimates the AMCEs each time as an
applied study would, and reports power, Type I error for null targets,
Type S and Type M errors (Gelman & Carlin, 2014) and coverage, each with
its Monte Carlo standard error. Respondent heterogeneity and respondent
subgroups can be added, and there is no limit on the number of
attributes.

| **WARNING** |
|:--:|
| This is a development version of the package. Version 0.3.0 changed the simulation model and the arguments of `power_sim()`, so results and code from earlier versions do not carry over unchanged; see the [release notes](https://github.com/albertostefanelli/cjsimPWR/blob/main/NEWS.md). If you find a bug, an inconsistency or an error, please open an issue on GitHub or drop a line at alberto.stefanelli(at)yale.edu. General suggestions on how to improve the package are also welcome. |

## Installation

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("albertostefanelli/cjsimPWR")
```

## Power for a conjoint design

Attributes are given by their numbers of levels (or level labels), and
AMCEs by one vector per attribute, one value per non-reference level.
The design below has three attributes with 2, 3 and 5 levels; 600
respondents each complete 5 tasks. One AMCE is zero so the output also
reports Type I error.

``` r
library(cjsimPWR)

power <- power_sim(
  levels    = c(2, 3, 5),
  true_amce = list(0.05, c(-0.05, 0), c(-0.03, -0.05, -0.05, 0.05)),
  units     = 600,
  n_tasks   = 5,
  seed      = 2114
)
power
#> Conjoint simulation: 1000 runs; logit DGP; CR1 / normal inference.
#>  type group attribute level true_amce null_status Power         Type I error  Type S        Type M
#>  amce <NA>  var_1     1      0.05     non-null    0.975 (0.005)               0.000 (0.000) 1.011 (0.008)
#>  amce <NA>  var_2     1     -0.05     non-null    0.903 (0.009)               0.000 (0.000) 1.066 (0.009)
#>  amce <NA>  var_2     2      0.00     exact                     0.049 (0.007)
#>  amce <NA>  var_3     1     -0.03     non-null    0.311 (0.015)               0.003 (0.003) 1.748 (0.019)
#>  amce <NA>  var_3     2     -0.05     non-null    0.684 (0.015)               0.000 (0.000) 1.199 (0.011)
#>  amce <NA>  var_3     3     -0.05     non-null    0.693 (0.015)               0.000 (0.000) 1.202 (0.011)
#>  amce <NA>  var_3     4      0.05     non-null    0.689 (0.015)               0.000 (0.000) 1.227 (0.011)
#>  Coverage      Runs (failed) Valid runs
#>  0.955 (0.007) 1000 (0)      1000
#>  0.965 (0.006) 1000 (0)      1000
#>  0.951 (0.007) 1000 (0)      1000
#>  0.950 (0.007) 1000 (0)      1000
#>  0.945 (0.007) 1000 (0)      1000
#>  0.943 (0.007) 1000 (0)      1000
#>  0.948 (0.007) 1000 (0)      1000
#> Monte Carlo standard errors in parentheses.
#> Power is for non-null targets; Type I error is for null targets, not nonsignificant estimates.
#> Measures are conditional on runs with valid inference.
```

Each row is one AMCE. `Power` is the share of the simulated experiments
in which the effect was significant at `alpha = 0.05` for a nonzero
target. Monte Carlo standard errors appear in parentheses; more runs
(`sim_runs`, default 1000) make the estimates more precise. `Type S` is
the share of significant estimates with the wrong sign and `Type M`
their average exaggeration factor. `Coverage` is the share of runs whose
95% confidence interval contains the population AMCE, evaluated using
its numerical reference.

Zero targets report `Type I error`, the false-positive rate, instead of
power. This measures how often the analysis finds a significant effect
when the true average effect is zero. For example, finding an effect in
5 out of 100 simulated studies where it is absent gives a Type I error
rate of 5%.

Zero-target subgroup differences labelled `calibrated` are only
approximately zero, so their reported Type I error is approximate.
Differences known to be exactly zero are labelled `exact`. The [Type I
error
guide](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/type_1_error.md)
explains the reporting, uncertainty and diagnostics, and [how to check
the false-positive rate of an
effect](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/type_1_error.md#checking-the-type-i-error-of-an-effect).

``` r
print(power$performance[, c("attribute", "level", "true_amce", "power", "power_mcse",
                            "type_1_error", "type_1_error_mcse", "type_m", "coverage", "n_valid")],
      digits = 3)
#>   attribute level true_amce power power_mcse type_1_error type_1_error_mcse type_m coverage n_valid
#> 1     var_1     1      0.05 0.975    0.00494           NA                NA   1.01    0.955    1000
#> 2     var_2     1     -0.05 0.903    0.00936           NA                NA   1.07    0.965    1000
#> 3     var_2     2      0.00    NA         NA        0.049           0.00683     NA    0.951    1000
#> 4     var_3     1     -0.03 0.311    0.01464           NA                NA   1.75    0.950    1000
#> 5     var_3     2     -0.05 0.684    0.01470           NA                NA   1.20    0.945    1000
#> 6     var_3     3     -0.05 0.693    0.01459           NA                NA   1.20    0.943    1000
#> 7     var_3     4      0.05 0.689    0.01464           NA                NA   1.23    0.948    1000
```

## Heterogeneous preferences

`sigma` is the standard deviation, across respondents, of each
respondent’s own AMCE on the probability scale: with `sigma = 0.05` and
an AMCE of 0.03, individual effects are spread with SD 0.05 around 0.03.
Heterogeneity leaves the average AMCEs unchanged but makes the choices
of a respondent more alike, which is what respondent-clustered standard
errors account for. As there is no generally valid value, use pilot
evidence and compare scenarios such as `sigma = c(0, 0.05, 0.10, 0.15)`
while keeping the AMCEs fixed. Details of the simulation model are in
[docs/simulation_model.md](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/simulation_model.md).

``` r
power_sim(levels = c(2, 3, 5), true_amce = list(0.05, c(-0.05, 0), c(-0.03, -0.05, -0.05, 0.05)),
          units = 600, n_tasks = 5, sigma = 0.05, seed = 2114)
```

## Subgroups

When the AMCEs are expected to differ between groups of respondents,
such as partisans (Kirkland & Coppock, 2018), give the group names, the
number of respondents in each group and one set of AMCEs per group.
`power_sim()` then reports the power for each group’s AMCEs and for the
difference between each group’s AMCE and that of the first group.

``` r
groups <- power_sim(
  levels    = c(2, 3),
  groups    = c("Democrat", "Republican"),
  units     = c(Democrat = 500, Republican = 300),
  n_tasks   = 4,
  true_amce = list(Democrat   = list(0.10, c(-0.05, 0.05)),
                   Republican = list(0.02, c(-0.05, 0.05))),
  seed      = 12
)
groups
#> Conjoint simulation: 1000 runs; logit DGP; CR1 / normal inference.
#>  type       group                 attribute level true_amce null_status target_error_bound Power
#>  amce       Democrat              var_1     1      0.10     non-null                       1.000 (0.000)
#>  amce       Democrat              var_2     1     -0.05     non-null                       0.743 (0.014)
#>  amce       Democrat              var_2     2      0.05     non-null                       0.738 (0.014)
#>  amce       Republican            var_1     1      0.02     non-null                       0.166 (0.012)
#>  amce       Republican            var_2     1     -0.05     non-null                       0.503 (0.016)
#>  amce       Republican            var_2     2      0.05     non-null                       0.519 (0.016)
#>  difference Republican - Democrat var_1     1     -0.08     non-null                       0.867 (0.011)
#>  difference Republican - Democrat var_2     1      0.00     calibrated  1.829e-07
#>  difference Republican - Democrat var_2     2      0.00     calibrated  1.823e-07
#>  Type I error  Type S        Type M        Coverage      Runs (failed) Valid runs
#>                0.000 (0.000) 0.996 (0.005) 0.933 (0.008) 1000 (0)      1000
#>                0.000 (0.000) 1.162 (0.010) 0.956 (0.006) 1000 (0)      1000
#>                0.000 (0.000) 1.158 (0.010) 0.948 (0.007) 1000 (0)      1000
#>                0.000 (0.000) 2.530 (0.033) 0.953 (0.007) 1000 (0)      1000
#>                0.000 (0.000) 1.395 (0.014) 0.952 (0.007) 1000 (0)      1000
#>                0.000 (0.000) 1.352 (0.013) 0.951 (0.007) 1000 (0)      1000
#>                0.000 (0.000) 1.065 (0.009) 0.948 (0.007) 1000 (0)      1000
#>  0.052 (0.007)                             0.948 (0.007) 1000 (0)      1000
#>  0.044 (0.006)                             0.956 (0.006) 1000 (0)      1000
#> Monte Carlo standard errors in parentheses.
#> Power is for non-null targets; Type I error is for null targets, not nonsignificant estimates.
#> Calibrated nulls report approximate Type I error; the target error bound is a reference confidence bound.
#> Measures are conditional on runs with valid inference.
```

A difference between two groups’ AMCEs compares the causal effects of
the same level for the two groups; it is not a test of overall
favourability, and it depends on the reference level (Leeper, Hobolt &
Tilley, 2020).

## Standard errors

AMCEs are estimated by least squares on the profile rows with standard
errors clustered by respondent, following Hainmueller, Hopkins and
Yamamoto (2014). The default (`vcov = "CR1"` with normal critical
values) reproduces the inference of `cjoint::amce()`. With few
respondents, or small subgroups, `inference = "t"` or `vcov = "CR2"`
with Satterthwaite degrees of freedom are available; when to use them is
discussed in
[docs/clustering.md](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/clustering.md).

## Why simulate?

Power can also be computed from the closed-form formulas of [Schuessler
& Freitag (2020)](https://doi.org/10.31235/osf.io/9yuhp), implemented in
their [cjpowR](https://github.com/m-freitag/cjpowR) package. They are
instant, and cjsimPWR gives the same answers where their assumptions
hold:

- Each effect is treated as if it were the only one shaping respondents’
  choices, which understates power when other attributes have sizeable
  effects.
- Every rated profile counts as an independent observation, which
  overstates power when respondents complete several tasks and differ in
  their preferences.
- The analysis is assumed to give 5% false positives and 95% confidence
  interval coverage at a 5% significance level. Simulation measures
  false positives and coverage directly, which is especially useful for
  designs with few respondents or small subgroups.

The first two err in opposite directions: for an AMCE of 0.05 and 2,000
rated profiles, the formula gives a power of 0.61, and simulation gives
between 0.48 and 0.65 depending on the design.
[docs/closed_form.md](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/closed_form.md)
explains each assumption in plain terms, with the simulations behind
these numbers.

A conjoint simulation can also be assembled in
[DeclareDesign](https://declaredesign.org/r/declaredesign/) (Blair et
al., 2019), which declares and diagnoses research designs of every kind.
Being general, it takes more work to set up for a conjoint experiment
and offers fewer conjoint-specific features than `power_sim()`;
[docs/declaredesign.md](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/declaredesign.md)
compares the two.

## References

Blair, G., Cooper, J., Coppock, A., & Humphreys, M. (2019). Declaring
and diagnosing research designs. *American Political Science Review*,
113(3), 838–859.

Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing
Type S (sign) and Type M (magnitude) errors. *Perspectives on
Psychological Science*, 9(6), 641–651.

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference
in conjoint analysis: Understanding multidimensional choices via stated
preference experiments. *Political Analysis*, 22(1), 1–30.

Kirkland, P. A., & Coppock, A. (2018). Candidate choice without party
labels. *Political Behavior*, 40(3), 571–591.

Leeper, T. J., Hobolt, S. B., & Tilley, J. (2020). Measuring subgroup
preferences in conjoint experiments. *Political Analysis*, 28(2),
207–221.

Schuessler, J., & Freitag, M. (2020). Power analysis for conjoint
experiments. SocArXiv. <https://doi.org/10.31235/osf.io/9yuhp>

Stefanelli, A., & Lukac, M. (2020). Subjects, trials, and levels:
Statistical power in conjoint experiments. SocArXiv.
<https://doi.org/10.31235/osf.io/spkcy>

Bugs and suggestions: open an issue on GitHub or write to
alberto.stefanelli(at)yale.edu.

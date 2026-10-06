# Power when you specify preferences directly: paired logit

Most cjsimPWR analyses start from the effects you expect to see: the average marginal component effects
(AMCEs), supplied through `true_amce`. Here you start from what drives choices instead. You describe how
respondents value a profile's features, including their own policy ratings or party identity, and the
package works out the effects that follow. This is useful when your theory and planned analysis use
those respondent characteristics. You can compare a paired logit, which models the choice between two
profiles directly, with standard AMCE regression on the same simulated experiments. If your assumptions
are already expressed as AMCEs, the existing `true_amce` interface remains the more direct starting point.

## Start with shared preferences

A profile's *utility* is its appeal to a respondent: add up each feature's score multiplied by the
weight the respondent gives it. Higher utility makes the profile more likely to be chosen. This first
model has the same weights for everyone, although respondents' ratings and parties differ. Preparing
its paired-coefficient targets takes well under a second on the test machine.

```r
library(cjsimPWR)
design <- conjoint_design(list(
  policy = c("left", "centre", "right"),
  party = c("A", "B", "C"), democracy = c("D+", "D-")))
model <- utility_model(design, terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2), coef = .3),
  term_match("party", share = c(.4, .3, .2), coef = .9),
  term_dummy("democracy", coef = -.8)))
model$truth[c("term", "input", "true_value")]
```

```text
           term input true_value
1 policy_rating   0.3        0.3
2   party_match   0.9        0.9
3     democracy  -0.8       -0.8
```

The policy term gives a profile 0.3 utility units for every rating point. The party term adds 0.9 when
its party matches the respondent's: 40% of respondents match A, 30% B, 20% C, and 10% match none. The
democracy term gives D− a weight of −0.8 relative to D+, the first and therefore reference level.
A positive weight makes a profile more likely to be chosen when its score is higher.

`input` is the coefficient you supplied. `true_value` is what the specified analysis would estimate
with unlimited data from this population. Here they agree because everyone has the same coefficients
and the analysis includes all the generating terms with the same coding. These are logit coefficients,
not percentage-point effects.

There are four ways to describe a feature:

| Term | What the profile receives |
| --- | --- |
| `term_dummy()` | One coefficient for each level other than the reference |
| `term_score()` | A fixed numeric score for each level, such as `scores = c(0, 1, 2)` |
| `term_match()` | Score 1 for the respondent's matching level and 0 otherwise |
| `term_rating()` | The respondent's rating of the displayed level |

Always put the terms inside `list()`, even for a single attribute. Values follow the design's level
order, or can be named by level. Every attribute needs a term; give it zero coefficient means and SDs
to make it inactive. Some combinations repeat the same information, such as a fixed score plus all
of that attribute's dummies, and cannot be estimated separately. See `?term_dummy` and `?utility_model`
for input validation and group-specific values.

## Choose plausible coefficients, ratings and variation

The coefficients describe changes in log-odds. To make their size concrete, consider profiles that
otherwise have equal utility. A D− coefficient of −0.8 gives the D− profile choice probability
`plogis(-0.8)`, about 31%, compared with 50% when tied. A party-match coefficient of 0.9 gives about
71%. With a policy-rating coefficient of 0.3, a two-point rating advantage gives `plogis(2 * .3)`,
about 65%. These are conditional comparisons; an AMCE averages over other attributes, opposing
profiles and respondents, so it is a different quantity.

Choose values from comparable data where possible, paying attention to the score units and reference
levels. A coefficient per point on a 0–10 rating scale cannot be reused unchanged after rescaling it
to 0–1. When evidence is limited, compare several substantively plausible values rather than treating
one convenient value as known. Increasing all coefficients does not increase every AMCE by the same
factor.

`coef_sd` describes disagreement about the weight, not variation in the ratings. For example,
`coef = -.8, coef_sd = .3` draws a democracy coefficient for each respondent from a normal distribution
with mean −0.8 and SD 0.3. About two thirds lie between −1.1 and −0.5. The default, zero, gives everyone
the same coefficient. Choose heterogeneity from evidence about individual differences, or compare
labelled scenarios; there is no universal recommended SD.

Ratings use their own `mean` and `sd`. They are continuous, unbounded normal draws: a mean of 4 and SD
of 2 can produce negative ratings or values above 10. Nothing is clipped or rounded. Use this feature
when that approximation makes sense for your ratings; it does not simulate an exact bounded response
scale. Each person's ratings, match and coefficients stay the same across all their tasks.

## Simulate and analyse one experiment

This example uses 500 respondents, each choosing between two profiles in five tasks. The added columns
show how the model scored the levels that were displayed. The negative policy rating in this output
also illustrates the unbounded normal rating draws.

```r
set.seed(1)
data <- simulate_experiment(design, units = 500, n_tasks = 5, model = model)
shown <- head(data[c("respondent", "task", "profile", "policy", "policy_rating", "party", "party_match")])
shown$policy_rating <- round(shown$policy_rating, 2)
shown

fit <- estimate_paired(data, design)
latent_estimates <- fit[c("term", "estimate", "std.error")]
latent_estimates[-1] <- lapply(latent_estimates[-1], round, 3)
latent_estimates

fit_probability <- estimate_paired(data, design, scale = "probability")
probability_estimates <- fit_probability[c("attribute", "level", "estimate", "std.error")]
probability_estimates[3:4] <- lapply(probability_estimates[3:4], round, 3)
probability_estimates
```

```text
  respondent task profile policy policy_rating party party_match
1          1    1       1   left          6.67     B           0
2          1    1       2   left          6.67     A           1
3          1    2       1  right         -0.46     C           0
4          1    2       2   left          6.67     C           0
5          1    3       1   left          6.67     C           0
6          1    3       2  right         -0.46     A           1
           term estimate std.error
1 policy_rating    0.273     0.023
2   party_match    1.092     0.082
3     democracy   -0.862     0.065
  attribute  level estimate std.error
1    policy centre   -0.057     0.011
2    policy  right   -0.109     0.013
3     party      B   -0.006     0.011
4     party      C   -0.034     0.010
5 democracy     D-   -0.177     0.013
```

The latent estimates are weights on the same scale as 0.3, 0.9 and −0.8 above. For an AMCE report,
choose `scale = "probability"`: these are the AMCEs implied by the paired model's predictions, expressed
as changes in choice probability relative to each attribute's reference level. For example, an estimate
of −0.06 is a reduction of six percentage points. This conversion uses the whole fitted model; it is
not `plogis()` applied to one coefficient.

Use the latent scale when your question concerns the weights themselves. Use the probability scale
when you want interpretable AMCEs or a comparison with ordinary AMCE regression. The default paired
analysis includes all the model's terms. Supplying an attribute in `terms` instead selects its dummies:
`terms = c("policy", "democracy")` asks a different question from using `"policy_rating"` and omits the
party term.

Standard errors allow observations from the same respondent to be related. CR1 is the default and
supports both scales; CR2 is also available for latent paired coefficients and ordinary AMCEs, but not
paired probability output. See the [clustering guide](clustering.md) for choosing an inference method.
The score columns expose only displayed levels: they cannot recover unseen ratings or distinguish
matching no party from matching a party never shown to that respondent.

## Compare power and choose a sample size

The following examples use 200 runs to keep the guide quick. Use 1,000 or more for an actual analysis,
and inspect the Monte Carlo standard errors. The first call evaluates the coefficient analysis at
1,000 respondents and five tasks each. It reuses the prepared targets and took about five seconds on
the test machine.

```r
latent <- power_sim(model = model, units = 1000, n_tasks = 5,
                    seed = 1, sim_runs = 200, estimators = "paired")
latent_power <- latent$performance[c("term", "true_value", "power", "power_mcse", "type_s", "type_m", "coverage")]
latent_power[-1] <- lapply(latent_power[-1], round, 3)
as.data.frame(latent_power)
```

```text
           term true_value power power_mcse type_s type_m coverage
1 policy_rating        0.3     1          0      0  1.004    0.935
2   party_match        0.9     1          0      0  0.999    0.930
3     democracy       -0.8     1          0      0  0.999    0.950
```

All 200 runs detect each coefficient with the correct sign. Type M is close to 1, so significant
estimates have little average exaggeration here, and coverage is about 93–95%. Power of 1 in this table
means every simulated run rejected; it does not establish that rejection is certain in a new experiment.
The reported MCSE is zero at this simulated boundary.

Now compare the two estimators on the probability scale. Requesting both selects this scale and prints
a message. This call also computes the missing AMCE targets once; use `amce = TRUE` in the original
`utility_model()` call if you want to prepare those in advance. Save `both$model` to retain work done
by the power call. The caller's original model is not modified automatically. This example, including
AMCE target construction, took about nine seconds.

```r
both <- power_sim(model = model, units = 1000, n_tasks = 5,
                  seed = 1, sim_runs = 200, estimators = c("amce", "paired"))
model <- both$model
amce_rows <- subset(as.data.frame(both$performance), estimator == "amce")
paired_rows <- subset(as.data.frame(both$performance), estimator == "paired")
comparison <- data.frame(
  effect = paste(amce_rows$attribute, amce_rows$level, sep = ": "),
  true_amce = amce_rows$true_value,
  power_amce = amce_rows$power, power_paired = paired_rows$power,
  sd_amce = amce_rows$emp_se, sd_paired = paired_rows$emp_se)
comparison[-1] <- lapply(comparison[-1], round, 3)
comparison
```

```text
          effect true_amce power_amce power_paired sd_amce sd_paired
1 policy: centre    -0.058      0.995        1.000   0.012     0.007
2  policy: right    -0.117      1.000        1.000   0.011     0.008
3       party: B    -0.018      0.320        0.755   0.013     0.007
4       party: C    -0.035      0.815        1.000   0.012     0.007
5  democracy: D-    -0.164      1.000        1.000   0.010     0.010
```

Both analyses nearly always detect the policy and democracy effects. For party B, the paired model
detects the effect in about 76% of runs, compared with 32% for AMCE regression. Its estimates also vary
less across runs: their SD is about 0.007 rather than 0.013. The democracy estimates have similar SDs
in this example. The tables distinguish power from precision: power may already be near 1 even when
one estimator has a smaller SD.

A gain in precision is possible because the paired analysis uses respondents' ratings and party matches,
which the ordinary AMCE regression does not. Its size depends on the population and analysis. The gain
also depends on the model being a useful description of choices: omitted or incorrectly coded features
can bias the paired probability estimates. A power analysis can examine that risk only when the
simulated assumptions include it. Neither estimator is guaranteed to be more efficient in every setting.

To compare sample sizes, keep the model and tasks fixed and change `units`. This short loop examines
power for the party-B AMCE at 250 and 500 respondents. It reuses the cached AMCE targets from `both`.
The same seed makes the comparison reproducible; each row still has only 200 runs. The loop took
about five seconds.

```r
sizes <- c(250, 500)
size_rows <- vector("list", length(sizes))
for (i in seq_along(sizes)) {
  result <- power_sim(model = model, units = sizes[i], n_tasks = 5,
                      seed = 1, sim_runs = 200, estimators = "paired", scale = "probability")
  model <- result$model
  party_b <- subset(as.data.frame(result$performance), attribute == "party" & level == "B")
  size_rows[[i]] <- data.frame(units = sizes[i], power = party_b$power,
                               power_mcse = party_b$power_mcse)
}
size_comparison <- do.call(rbind, size_rows)
size_comparison[2:3] <- lapply(size_comparison[2:3], round, 3)
size_comparison
```

```text
  units power power_mcse
1   250 0.260      0.031
2   500 0.455      0.035
```

A sample-size decision should consider every effect you need to estimate, its precision and the cost
of extra respondents or tasks, not just the strongest effect's power. Prepared models can be reused
with different sample sizes, task counts, experiment seeds and worker counts.

## Let respondents differ in their weights

When coefficients vary, computing population targets may take minutes at the default budget of
2,097,152 paired cells per group per batch. Prepare a model once and reuse it. The example below uses
**131,072 cells** for a faster exploratory calculation; this is an explicit smaller budget, not a change
to the default. It took about 10 seconds on the test machine. Its `reference_half_width` reports the
uncertainty in each computed target. Increase precision when that uncertainty matters for bias, coverage or Type S/M.

```r
heterogeneous <- utility_model(design, terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2), coef = .3, coef_sd = .1),
  term_match("party", share = c(.4, .3, .2), coef = .9),
  term_dummy("democracy", coef = -.8, coef_sd = .3)),
  control = list(paired_cells = 131072))
heterogeneous_truth <- heterogeneous$truth[c("term", "input", "true_value", "reference_half_width")]
heterogeneous_truth[-1] <- lapply(heterogeneous_truth[-1], round, 6)
heterogeneous_truth
```

```text
           term input true_value reference_half_width
1 policy_rating   0.3   0.287492             0.000430
2   party_match   0.9   0.884827             0.001036
3     democracy  -0.8  -0.780469             0.001241
```

The policy target is about 0.287 rather than the input of 0.3; the democracy target is about −0.780
rather than −0.8. Their reference half-widths are about 0.00043 and 0.00124, respectively, so this
exploratory calculation distinguishes these changes from Monte Carlo noise. These half-widths describe
uncertainty in the population targets, not the standard errors of estimates from an experiment.

When respondents have different weights, a single fitted logit coefficient summarizes their choices;
it need not equal the mean weight entered as `coef`. Even the party target can change while its own
coefficient is fixed, because the policy and democracy coefficients vary. Targets can move in either
direction across models; a shift towards zero in this example is not a general rule.

The package measures bias, coverage and Type S/M against `true_value`, the target appropriate for the
selected analysis. On the probability scale that target is always the true population AMCE, so a
misspecified paired model's bias is included. The target calculation depends on the model and
integration controls, not on the number of respondents or experiment seed.

## Compare groups and understand exact zeros

Group-specific inputs use lists named by group. Here two groups give opposite fixed ratings to the
policy positions but have the same rating weight and the same democracy weight. This small example
is integrated exactly and runs quickly.

```r
opposite_design <- conjoint_design(list(policy = c("left", "right"),
                                       democracy = c("D+", "D-")))
opposite <- utility_model(opposite_design, groups = c("A", "B"), amce = TRUE,
  terms = list(
    term_rating("policy", mean = list(A = c(0, 1), B = c(1, 0)), sd = 0, coef = 1),
    term_dummy("democracy", coef = -.8)))
opposite_truth <- opposite$truth[c("scale", "group", "attribute", "level", "true_value", "null")]
opposite_truth$true_value <- round(opposite_truth$true_value, 6)
opposite_truth
```

```text
         scale group attribute level true_value  null
1       latent     A    policy  <NA>   1.000000 FALSE
2       latent     A democracy    D-  -0.800000 FALSE
3  probability     A    policy right   0.217525 FALSE
4  probability     A democracy    D-  -0.172066 FALSE
5       latent     B    policy  <NA>   1.000000 FALSE
6       latent     B democracy    D-  -0.800000 FALSE
7  probability     B    policy right  -0.217525 FALSE
8  probability     B democracy    D-  -0.172066 FALSE
9       latent B - A    policy  <NA>   0.000000  TRUE
10      latent B - A democracy    D-   0.000000  TRUE
11 probability B - A    policy right  -0.435050 FALSE
12 probability B - A democracy    D-   0.000000  TRUE
```

Both coefficient differences are exactly zero: the groups share weights of 1 and −0.8. Their democracy
AMCE difference is also exactly zero, because exchanging the policy levels leaves the democracy effect
unchanged. These three differences are certified nulls. The policy AMCEs have opposite signs, so their
difference is −0.43505 and is not a null.

With random coefficients, each distinct group's targets are calculated separately and can differ by
Monte Carlo noise even when a symmetry proves a particular difference is zero. The package reports
that difference as exactly zero. It never certifies a null just because a computed number is small.
`null = FALSE` means no zero was proved, not that a nonzero effect was proved.

A zero population mean does not imply that everyone is unaffected. For example, respondents who rate
two policy levels equally *on average* can still have positive or negative personal policy effects.
A certified zero AMCE therefore retains any respondent-level effect SD and its uncertainty.

## Read the results

`power` is the share of valid runs rejecting a zero effect when the target is not certified null.
For a certified null, the same rejection rate is reported as `type_1_error`: a false positive.
`power_mcse` and the other `_mcse` columns show uncertainty from using a finite number of simulation
runs. For example, power near 0.5 with 200 valid runs has an MCSE of about 0.035.

`type_s` is the fraction of significant estimates with the wrong sign. `type_m` is their average
exaggeration ratio: 1 means the estimated magnitude matches the target on average, and 2 means twice
the target's magnitude. They condition on significant results and are undefined for a zero target.
`coverage` is the proportion of confidence intervals containing the target; a nominal 95% interval
should have coverage near 0.95 when the inference works well. The analytic Type S/M columns provide a
simplified normal-theory benchmark, not additional simulation results. See the
[error-rate guide](error_rates.md) for definitions and interpretation.

An unresolved-sign warning means the numerical target is too uncertain or too close to zero for its
sign and exaggeration ratios to be reliable. Type S/M remain in the result where defined; do not treat
them as settled answers. The warning identifies the affected effects and distinguishes reference
uncertainty from numerical limitations. More reference simulation can help the former, but cannot
settle an effect that is exactly zero. Review the assumptions and, where available, the symmetry or
analytic proof for such an effect.

A fit the package cannot verify is reported as failed (`nonconvergence`) and excluded from that
effect's summaries; it is never counted as nonsignificant. A failed paired analysis does not discard
valid AMCE estimates from the same experiment. Check `n_failed`, `n_valid` and `$failures`. A failed
group affects its own estimates and contrasts using it, while unrelated groups can retain inference.

## Limits and practical cautions

- Profiles are independently and uniformly randomized over each attribute's levels. The simulator
  does not implement restricted assignment, unequal level probabilities, candidate relabelling,
  survey weights, opt-outs or pooled-group analyses.
- Ratings and coefficient deviations are independent normal draws within group. Correlated
  preferences, bounded ratings, observed respondent tables and multiple simultaneous matches are
  not supported. This is a single-logit analysis of potentially heterogeneous respondents, not a
  mixed-logit estimator.
- Probability conversion is an AMCE interpretation under the independent uniform assignment
  assumptions. With other assignment schemes, the standalone estimator supplies the stated
  regression projection of its predictions.
- Paired power requires a latent prepared model. AMCE-only power accepts a calibrated or latent one.
  Latent `coef_sd` can differ by term and group; calibrated AMCE-scale `sigma` still has its existing
  scalar interface. The package does not calibrate these latent models to requested AMCEs.
- Coefficient ratios and equal-rating democracy penalties are not power targets here. Under
  heterogeneity, a fitted ratio need not be the average respondent's trade-off. See the
  [trade-off proposal](TODO.md) for the separate questions that still need an implementation.

## Technical appendix

### Analysis targets, output names and omitted attributes

Paired predictors are profile-1 minus profile-2 scores, with no intercept. With fixed coefficients and
all generating columns present with the same coding, the population logit target equals the input
coefficient vector. Reordering columns preserves that result; recoding changes it. For example, a
binary score `c(0, 2)` with weight 0.3 has a dummy coefficient of 0.6. Under heterogeneity or an altered
analysis the target solves the population logit score equations, rather than copying coefficient means.

A dummy-only paired analysis covering every attribute gives the same probability estimates and CR1
covariance as ordinary AMCE regression whenever both fits are finite and estimable. Probability CR1
includes uncertainty from both fitted logit coefficients and sampled profiles/respondents. CR2 uses
verified group fits and supporting models for contrasts; requesting paired probability CR2 is an error.
CR1 finite-sample corrections and `inference = "t"` use full-sample counts, including failed groups.
Each group supporting an effect still needs at least two respondents.

Latent-model power tables use `true_value` on both reporting scales; existing calibrated results use
`true_amce`. Check `scale` and `estimator` when combining tables. Standalone `estimate_paired()` follows
`estimate_amce()` for group order: factor levels, or first appearance for character groups. Reordering
a grouped data frame can therefore change the reference group when `groups` is omitted. Pass
`groups = model$groups` to keep the prepared order explicit.

An attribute name in `paired_terms` selects its full dummy block; a generated score-column name selects
that term. Probability output retains rows for **all** design AMCEs, including attributes omitted from
the paired predictors. For example, `paired_terms = c("policy_rating", "democracy")` omits party.
The package emits a message and marks its paired probability rows `in_analysis = FALSE` in `$truth`
and `$performance`. Predictions do not use party, but those rows' bias and coverage still use the full
population party AMCE. Their rejection rates should not be read as the ability to recover the omitted
attribute's true effect. Ordinary AMCE rows are unchanged. `in_analysis = TRUE` only says that at least
one analysis term uses the attribute; it is not a guarantee of correct specification.

### Prepared-model caches

`model$truth` holds the default paired analysis and any computed AMCE truths. Missing AMCE targets
are filled when power first needs them. Custom latent-analysis truths are kept separately in
`model$analysis_truths`, keyed by the ordered `paired_terms`; they do not replace the default truth.
Both caches are returned in `result$model`. Retain that model before another call, including in loops,
to avoid repeating target calculations. Treat a prepared model as immutable: changing its coefficient
or control fields by hand can invalidate its cached truths; construct a new model for changed assumptions.
Caches affect reference computation, not the experiment's profile or choice draws.

### Certification and sign diagnostics

Certification uses sufficient proofs, never numerical closeness or a confidence interval containing
zero. Rules include identical utility at a level and its reference, exchangeable level distributions,
supported binary-symmetry cases, inactive independent predictors, and exact matching-model coefficients.
Group differences may be certified from shared targets, two certified zeros, equal analytic coefficients,
or supported level-permutation/sign symmetries of the full specification.

A symmetry comparison must use one common level permutation for all terms on an attribute. It matches
coefficient means and SDs, rating distributions, fixed scores and canonical match probabilities exactly,
including no-match probability. A sign reversal changes coefficient means, not SDs or respondent score
distributions. For an AMCE target, the target attribute's permutation must preserve its reference and
compared level and cannot reverse its utility sign; other attributes may be permuted or sign-reversed.
For a latent score/rating/match coefficient, a permutation may also act on its target attribute; a dummy
target must retain the compared levels. Paired analyses require complete dummy blocks for this proof.
These conditions are sufficient, not a complete catalogue of all possible zero differences.

Reported certified zeros have zero mean-reference uncertainty. Their AMCE SD and its uncertainty remain
unless the individual contrast itself is identically zero. Numerical paired diagnostics retain the raw
coefficient solution, reference covariance, scaled scores and Newton corrections; substituting certified
zeros in the report does not rerun the solver or change that raw solution.

`sign_resolved` is `NA` for certified nulls and `TRUE` for analytic nonzero targets. Other rows compare
absolute target size against their own reference half-width, solver correction and roundoff guard.
For contrasts, errors are combined in the coefficients' original units. These are numerical guards,
not proved bounds on approximation error. A numerically unresolved exact-zero AMCE may involve only
roundoff, without any paired solver error.

### Integration, precision and runtime

Finite-support integration is exact when coefficient/rating SDs are zero and the profile-reference and
joint match-cell limits permit enumeration. Otherwise, paired targets use independent respondent and
ordered-profile-pair cells. The same cells are replayed for a shared damped Newton solve in bounded
blocks. AMCE integration uses independent reference streams and antithetic respondent nodes with the
existing forward AMCE transform. A population Gram check tests identification analytically, including
rare predictors; a rank-deficient sampled reference raises a separate budget diagnostic.

`control` accepts `seed`, `exact_max_pairs`, `exact_max_rows`, `exact_max_cells`, `paired_cells`,
`verification_pairs`, `verification_draws` and `verification_batches`. `paired_cells` is per group per
batch and is independent of the AMCE respondent/profile budgets. Budgets are fixed, with no
acceptance-based reference resampling. The solver starts from mapped coefficient means when possible; if that start fails, it retries from zero on the same fixed cells.
If neither solve succeeds, it raises an error rather than returning its last iterate as a truth.

The default is **2,097,152 paired cells per group per batch**, with 16 batches. It was the first of five
candidate budgets to meet a 0.001 standardized latent half-width target in all seven development
scenarios. All seven also passed an interval-overlap check at 4,194,304 cells: each raw coefficient
change was smaller than the sum of its two half-widths. This is stability evidence, not a universal
bound on integration bias.

The table gives **historical budget-selection timings before the later solver warm starts**,
measured on an Apple M1 Pro, eight logical CPUs, R 4.2.1 and OpenBLAS 0.3.29. Scenarios
ran concurrently. It records how the default was chosen, not current runtime predictions. The original
heterogeneous guide run took 414.1 seconds, versus 370.4 for its budget-benchmark counterpart under a
different concurrent workload; neither measures the fast fixed-coefficient example above.

| Historical scenario | Largest standardized half-width | Construction seconds |
| --- | ---: | ---: |
| Three-attribute heterogeneous example | 0.000267 | 370.4 |
| Weak coefficient heterogeneity | 0.000055 | 227.7 |
| Two-attribute heterogeneous rating model | 0.000197 | 124.1 |
| Rare party matches | 0.000248 | 218.5 |
| Omitted party analysis | 0.000341 | 207.9 |
| Two groups with different rating distributions | 0.000689 | 438.8 |
| Five-attribute model | 0.000325 | 380.5 |

Reference MCSEs and nominal simultaneous 99% batch-t intervals use separate families for each paired
latent analysis and for the model's AMCE means/SDs across groups. Distinct base-group quantities count
once, including analytic and certified-null quantities; shared-reference copies and contrasts add no
comparisons. The 99% statement applies within a family, not jointly across analyses and families.
Independent-group MCSEs combine in quadrature; their half-widths add. Both probability estimators reuse
the same AMCE family. Adding another reference family or using a cache cannot widen an existing family's
intervals or change its sign flags.

These intervals describe Monte Carlo error, not solver error or integration bias. Reference uncertainty
is included in `bias_mcse` and the coverage-sensitivity columns. It does not change the observed fraction
of experiment fits rejecting zero; null classification comes from proofs. More experiment runs reduce
uncertainty in estimated rejection rates, but do not improve the population target's precision.

### Random streams and reproducibility

Reference calculations depend on model inputs and integration controls, never sample size or experiment
seed. Group roots use separate L'Ecuyer streams; within each root, paired batch b uses substream
`2*b - 1` and AMCE batch b uses `2*b`. Experiment streams retain substream zero, including when truth and
experiment seeds have equal values. Identical group specifications share their first occurrence's
reference; symmetry-certified differences need not share the two groups' numerical references.

Analysis choices, analytic shortcuts and caches do not shift those allocations. Truth calculations and
`power_sim()` restore the caller's RNG, including Box-Muller's cached state; `simulate_experiment()`
advances it. Results agree across worker counts and longer run counts retain earlier experiment runs.
`seed` in `power_sim()` controls experiments; `control$seed` in `utility_model()` controls target
integration. Changing either seed is not a way to repair a poorly specified model.

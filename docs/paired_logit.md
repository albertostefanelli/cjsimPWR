# Paired logit

## Design

`conjoint_design()` names the attributes and lists their levels. The first level of each attribute is
the reference level that the other levels are compared with.

```r
library(cjsimPWR)
design <- conjoint_design(list(
  policy    = c("left", "centre", "right"),
  party     = c("A", "B", "C"),
  democracy = c("D+", "D-")))
```

`D+` is a candidate who respects democratic rules and `D-` one who does not.

## Utility terms

Each attribute gets a term that says what a profile receives for it. A profile's utility is the sum
of what it receives from all terms, and in each task the profile with the higher utility is the more
likely to be chosen: a little more likely when the difference is small, almost certainly when it is
large.[^logit] `coef` is the weight of a term, what one unit of its score adds to utility. Zero means
the attribute does not matter, and a negative value counts against the profile.

[^logit]: Profile 1 is chosen with probability `plogis(U1 - U2)`, the logistic function of the
    utility difference. This is the choice model of the
    [simulation model guide](simulation_model.md#choice-model), and the reason the analysis is called
    a paired logit.

| Term | Score of a profile | Example |
| --- | --- | --- |
| `term_dummy()` | 1 for the non-reference level shown, 0 otherwise; one `coef` per non-reference level | A candidate who breaks the rules loses 0.8 |
| `term_score()` | A fixed number assigned to each level in `scores` | Years of experience 0, 5 and 10, each year worth 0.05 |
| `term_match()` | 1 when the level is the respondent's own, 0 otherwise; `share` gives the proportion of respondents matching each level | A candidate from the respondent's party gains 0.9 |
| `term_rating()` | The respondent's own rating of the level, drawn from a normal distribution with `mean` and `sd` | A policy the respondent rates 7 adds 7 × 0.3 |

```r
model <- utility_model(design, terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2), coef = 0.3),
  term_match("party", share = c(0.4, 0.3, 0.2), coef = 0.9),
  term_dummy("democracy", coef = -0.8)))
model
```

```text
Latent utility model (logit)
Attributes (reference level first): policy [left, centre, right]; party [A, B, C]; democracy [D+, D-]
Groups: ungrouped

Coefficient means and SDs:
          term   type attribute level latent_mean latent_sd
 policy_rating rating    policy  <NA>         0.3         0
   party_match  match     party  <NA>         0.9         0
     democracy  dummy democracy    D-        -0.8         0

Population targets:
 estimator  scale        type        target input true_value  null
    paired latent coefficient policy_rating   0.3        0.3 FALSE
    paired latent coefficient   party_match   0.9        0.9 FALSE
    paired latent coefficient democracy[D-]  -0.8       -0.8 FALSE
See $truth for full targets and precision, $parameters for coefficients, and $diagnostics for numerical details.
```

- `policy`: every respondent has their own rating of each position. Ratings of `left` average 6 with
  an `sd` of 2, so about two thirds of respondents rate it between 4 and 8; `centre` averages 5 and
  `right` 4. A profile adds 0.3 for every rating point.
- `party`: 40 percent of respondents belong to party A, 30 percent to B and 20 percent to C; the
  remaining 10 percent belong to none. A profile from the respondent's own party adds 0.9.
- `democracy`: `D-` subtracts 0.8 compared with `D+`, the reference level.

A respondent keeps the same ratings, party and coefficients in every task. Terms always go inside
`list()`, even a single one. Every attribute needs a term; `term_dummy("x", coef = 0)` switches an
attribute off. Values follow the design's level order, or can be named by level. A dummy term with
several non-reference levels takes one `coef` per level: `term_dummy("policy", coef = c(-0.2, -0.5))`.
Group-specific values are shown under [Groups](#groups).

In the population targets, `input` is the coefficient you supplied and `true_value` is what the
analysis would recover from this population with an unlimited number of respondents. They are equal
here; they differ when coefficients vary across respondents (see
[Heterogeneous coefficients](#heterogeneous-coefficients)).

## Choosing coefficients

Coefficients have no natural unit. To judge their size, compare two profiles that are equal in every
other respect and give one a one-unit score advantage on the term; `plogis(coef)` is the probability
that it is chosen.

| `coef` | `plogis(coef)` |
| ---: | ---: |
| 0 | 0.50 |
| 0.2 | 0.55 |
| 0.5 | 0.62 |
| 0.8 | 0.69 |
| 1.0 | 0.73 |
| 1.5 | 0.82 |
| 2.0 | 0.88 |

Negative coefficients work the other way: at −0.8 the rule-breaking candidate is chosen with
probability `plogis(-0.8)`, about 0.31, against an otherwise identical candidate. For a rating term
the advantage is per rating point: a two-point lead at `coef = 0.3` gives `plogis(0.6)`, about 0.65.
These compare two profiles that differ in one thing only. The AMCE, which averages over the other
attributes, the competing profile and the respondents, is smaller; the package computes it for you.

Take coefficients from earlier studies or a pilot where possible, and mind the units: a coefficient
per point on a 0–10 scale is ten times one per point on a 0–1 scale. Without evidence, compare a few
plausible values rather than treating one as known.

`mean` and `sd` in `term_rating()` describe the ratings. They are normal draws and are not clipped or
rounded, so a mean of 4 with `sd = 2` produces some ratings below 0 and some above 10; a bounded
rating scale is only approximated. `coef_sd`, zero by default, lets the coefficient itself vary across
respondents; see [Heterogeneous coefficients](#heterogeneous-coefficients).

## Simulating and estimating one experiment

`simulate_experiment()` draws one data set from the model: here 500 respondents with five tasks each
and two profiles per task.

```r
set.seed(1)
data <- simulate_experiment(design, units = 500, n_tasks = 5, model = model)
print(head(data[c("respondent", "task", "profile", "policy", "policy_rating",
                  "party", "party_match", "democracy", "y")]), digits = 3)
```

```text
  respondent task profile policy policy_rating party party_match democracy y
1          1    1       1   left          6.67     B           0        D+ 0
2          1    1       2   left          6.67     A           1        D- 1
3          1    2       1  right         -0.46     C           0        D+ 0
4          1    2       2   left          6.67     C           0        D+ 1
5          1    3       1   left          6.67     C           0        D- 1
6          1    3       2  right         -0.46     A           1        D+ 0
```

Each row is one profile, and `y` is 1 for the chosen one. Respondent 1 rates `left` 6.67 and `right`
−0.46 (an unclipped rating below zero) and belongs to party A; in their first task they chose a `D-`
candidate of their own party over a `D+` candidate of party B. The simulation adds a score column for
each rating, score or match term (`policy_rating`, `party_match`). Real data need such columns before
the paired logit can use ratings or matches.

For either estimator, pass a `cj_design` that declares the full intended attribute levels and
references, and reuse it when fitting so the comparisons remain explicit even if a level is absent
from the sample. The data must be a nonempty data frame with one row per profile and unique column
names. AMCE data need finite numeric `y`, non-missing `respondent`, and one value per profile for
each design attribute; add a non-missing `group` column for subgroup estimation. Missing outcomes
or attributes, undeclared values, and malformed attribute columns stop with an error. Paired data
also need `task` and `profile`, with exactly one profile 1 and one profile 2 per respondent-task and
complementary binary outcomes. If this check fails, inspect outcome coding and profile pairing.
Invalid data stop with an error. Non-estimable effects, nonconvergent fits, or failed inference
instead retain result rows with a failure status.

For external scores or ratings, include one finite numeric value per profile and select their
columns by name with `terms =`. For example, to fit all terms in this model, use
`estimate_paired(data, design, terms = c("policy_rating", "party_match", "democracy"))`. A missing
selected score column and a present but invalid score column are distinct input errors; add or repair
the column before fitting.

`estimate_paired()` fits the paired logit. By default it uses all the model's terms and reports their
coefficients:

```r
fit <- estimate_paired(data, design)
print(fit[c("term", "estimate", "std.error")], digits = 3)
```

```text
           term estimate std.error
1 policy_rating    0.273    0.0227
2   party_match    1.092    0.0823
3     democracy   -0.862    0.0653
```

The estimates are close to the coefficients that generated the data, 0.3, 0.9 and −0.8. The standard
error is how far an estimate typically lands from the true value; an estimate is significant at the
5 percent level when it is at least about twice its standard error. Standard errors allow the choices
of one respondent to be related (clustered by respondent, CR1 by default; see the
[clustering guide](clustering.md)).

`scale = "probability"` reports the same fit as AMCEs (Hainmueller, Hopkins and Yamamoto 2014):
changes in the probability of being chosen when a level replaces its attribute's reference level,
averaged over respondents and the other attributes.

```r
fit_probability <- estimate_paired(data, design, scale = "probability")
print(fit_probability[c("attribute", "level", "estimate", "std.error")], digits = 3)
```

```text
  attribute  level estimate std.error
1    policy centre -0.05735    0.0106
2    policy  right -0.10857    0.0126
3     party      B -0.00628    0.0110
4     party      C -0.03437    0.0100
5 democracy     D- -0.17655    0.0134
```

A `D-` candidate is chosen about 18 percentage points less often than a `D+` one, and a `right`
candidate about 11 points less often than a `left` one, averaged over respondents' ratings. The policy
rows compare the levels shown, not a one-point change in a rating. The party rows are small because B
and C differ from A only in having fewer respondents who belong to them.

Use `scale = "latent"`, the default, when the question is about the coefficients, as in a trade-off.
Use `scale = "probability"` to report AMCEs or to compare with `estimate_amce()`.

## Power

`power_sim(model = )` simulates the experiment `sim_runs` times, fits the paired logit each time and
reports the share of runs in which each coefficient is significant: its power. `estimators = "paired"`
asks for the paired logit alone, on the latent scale.

```r
latent <- power_sim(model = model, units = 1000, n_tasks = 5,
                    seed = 1, sim_runs = 200, estimators = "paired")
latent
```

```text
Conjoint simulation: 200 runs; logit DGP; CR1 / normal inference.
Estimators: paired; scale: latent
 estimator scale  type        term          attribute level true_value Power         Type S        Type M        Coverage      Runs (failed) Valid runs
 paired    latent coefficient policy_rating policy    <NA>   0.3       1.000 (0.000) 0.000 (0.000) 1.004 (0.003) 0.935 (0.017) 200 (0)       200
 paired    latent coefficient party_match   party     <NA>   0.9       1.000 (0.000) 0.000 (0.000) 0.999 (0.004) 0.930 (0.018) 200 (0)       200
 paired    latent coefficient democracy     democracy D-    -0.8       1.000 (0.000) 0.000 (0.000) 0.999 (0.004) 0.950 (0.015) 200 (0)       200
Monte Carlo standard errors in parentheses.
Measures are conditional on runs with valid inference.
```

With 1,000 respondents, each coefficient was significant in all 200 runs. `Type S` is the share of
significant estimates with the wrong sign and `Type M` their average exaggeration (1.0 means none,
1.5 means 50 percent too large); both are harmless at high power and grow as power falls. `Coverage`
is how often the 95 percent confidence interval contained `true_value`; it should be near 0.95. The
numbers in parentheses are Monte Carlo standard errors, how much each figure could move with a
different set of 200 runs; a power of 1.000 (0.000) means every run rejected, not that rejection is
certain. `Runs (failed)` counts runs whose fit could not be verified; they are excluded from the
other columns, and `latent$failures` gives the reasons. `latent$performance` holds the same table,
with the Monte Carlo standard errors in `_mcse` columns.

## Comparing with AMCE regression

`estimators = c("amce", "paired")` runs ordinary AMCE regression and the paired logit on the same
simulated experiments. Both are then reported on the probability scale, so that they estimate the
same AMCEs; `power_sim()` prints a message to say so, and another reminding you that the policy and
party rows compare levels, not rating points or the match coefficient.

```r
both <- power_sim(model = model, units = 1000, n_tasks = 5,
                  seed = 1, sim_runs = 200, estimators = c("amce", "paired"))
model <- both$model
amce_rows <- subset(as.data.frame(both$performance), estimator == "amce")
paired_rows <- subset(as.data.frame(both$performance), estimator == "paired")
data.frame(
  effect = paste(amce_rows$attribute, amce_rows$level, sep = ": "),
  true_amce = round(amce_rows$true_value, 3),
  power_amce = amce_rows$power, power_paired = paired_rows$power,
  sd_amce = round(amce_rows$emp_se, 3), sd_paired = round(paired_rows$emp_se, 3))
```

```text
          effect true_amce power_amce power_paired sd_amce sd_paired
1 policy: centre    -0.058      0.995        1.000   0.012     0.007
2  policy: right    -0.117      1.000        1.000   0.011     0.008
3       party: B    -0.018      0.320        0.755   0.013     0.007
4       party: C    -0.035      0.815        1.000   0.012     0.007
5  democracy: D-    -0.164      1.000        1.000   0.010     0.010
```

AMCE regression detects the small party-B effect in 32 percent of runs, the paired logit in 76
percent. `emp_se`, the standard deviation of the estimates across runs, shows why: the paired
estimates vary about half as much. The paired logit uses each respondent's ratings and party match;
AMCE regression does not.

The gain depends on the terms describing choices correctly. If respondents care about something the
terms leave out, or about an attribute in a different way, the paired probability estimates can be
biased while AMCE regression is unaffected. A power analysis shows this only if the mismatch is built
into the simulated model.

`model <- both$model` keeps the AMCE population targets that this call computed, so later calls do
not repeat the work. `amce = TRUE` in `utility_model()` computes them up front.

## Sample size

Hold the model and `n_tasks` fixed and compare values of `units`. This loop reports power for the
party-B AMCE at 250 and 500 respondents, reusing the cached targets.

```r
sizes <- c(250, 500)
rows <- vector("list", length(sizes))
for (i in seq_along(sizes)) {
  result <- power_sim(model = model, units = sizes[i], n_tasks = 5,
                      seed = 1, sim_runs = 200, estimators = "paired", scale = "probability")
  model <- result$model
  party_b <- subset(as.data.frame(result$performance), attribute == "party" & level == "B")
  rows[[i]] <- data.frame(units = sizes[i], power = party_b$power, power_mcse = party_b$power_mcse)
}
print(do.call(rbind, rows), digits = 2)
```

```text
  units power power_mcse
1   250  0.26      0.031
2   500  0.46      0.035
```

Base the decision on the smallest effect you need to detect, and check every effect you plan to
report. When coefficients vary across respondents, extra respondents raise power more than extra
tasks do. Near the target power, increase `sim_runs` before choosing between neighbouring sizes: with
200 runs the figures above have Monte Carlo standard errors of about 0.03.

## Heterogeneous coefficients

`coef_sd` lets a coefficient vary across respondents. With `coef = -0.8, coef_sd = 0.3`, each
respondent's democracy coefficient is drawn from a normal distribution with mean −0.8 and SD 0.3, so
about two thirds of respondents lie between −1.1 and −0.5.

With varying coefficients, the population targets are computed by simulation, which takes minutes at
the default precision. `control = list(paired_cells = 131072)` lowers the precision to finish in
about ten seconds; use it to explore and the default for final numbers.

```r
mixed <- utility_model(design, terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2), coef = 0.3, coef_sd = 0.1),
  term_match("party", share = c(0.4, 0.3, 0.2), coef = 0.9),
  term_dummy("democracy", coef = -0.8, coef_sd = 0.3)),
  control = list(paired_cells = 131072))
print(mixed$truth[c("term", "input", "true_value", "reference_half_width")], digits = 3)
```

```text
           term input true_value reference_half_width
1 policy_rating   0.3      0.287              0.00043
2   party_match   0.9      0.885              0.00104
3     democracy  -0.8     -0.780              0.00124
```

`true_value` now differs from `input`. The paired logit fits one coefficient to everyone's choices,
and that coefficient is not the mean of the individual coefficients; here it is closer to zero, but it
can move either way. The party target moved although its own coefficient is fixed, because the other
two vary. Bias and coverage are measured against `true_value`, the target of the fitted analysis.
`reference_half_width` is the precision of a simulated target: the policy target is 0.287 ± 0.0004,
so its shift from 0.3 is not simulation noise.

Prepare such a model once and reuse it. Its targets do not depend on `units`, `n_tasks` or the
experiment `seed`.

## Groups

`groups` names respondent groups. Inputs that differ between groups are given as lists named by
group; the others are shared. Here group A rates `right` one point above `left`, group B the reverse,
and both groups have the same coefficients. The design is small enough for the targets to be computed
exactly.

```r
small <- conjoint_design(list(policy = c("left", "right"), democracy = c("D+", "D-")))
grouped <- utility_model(small, groups = c("A", "B"), amce = TRUE, terms = list(
  term_rating("policy", mean = list(A = c(0, 1), B = c(1, 0)), sd = 0, coef = 1),
  term_dummy("democracy", coef = -0.8)))
print(grouped$truth[c("scale", "group", "attribute", "level", "true_value", "null")], digits = 3)
```

```text
         scale group attribute level true_value  null
1       latent     A    policy  <NA>      1.000 FALSE
2       latent     A democracy    D-     -0.800 FALSE
3  probability     A    policy right      0.218 FALSE
4  probability     A democracy    D-     -0.172 FALSE
5       latent     B    policy  <NA>      1.000 FALSE
6       latent     B democracy    D-     -0.800 FALSE
7  probability     B    policy right     -0.218 FALSE
8  probability     B democracy    D-     -0.172 FALSE
9       latent B - A    policy  <NA>      0.000  TRUE
10      latent B - A democracy    D-      0.000  TRUE
11 probability B - A    policy right     -0.435 FALSE
12 probability B - A democracy    D-      0.000  TRUE
```

`amce = TRUE` computes the AMCE targets up front, so both scales appear. The `B - A` rows are the
differences between groups, which `power_sim()` also tests. Three are marked `null = TRUE`: the
package has proved from the inputs that they are exactly zero, because the groups share their
coefficients and swapping the policy levels leaves the democracy effect unchanged. Such rows report
`type_1_error`, the share of runs that wrongly find a difference, instead of power. The policy AMCEs
have opposite signs, so their difference is not zero. A zero is never certified because a computed
number is small; `null = FALSE` means no zero was proved, not that an effect was.

`units` and `n_tasks` can be given per group: `units = c(A = 300, B = 300)`.

## Assumptions

- Profiles are drawn independently and uniformly over each attribute's levels. Restricted or
  weighted randomization, more than two profiles per task, opt-outs and survey weights are not
  simulated.
- Ratings and coefficient deviations are independent normal draws within group. A respondent matches
  at most one level of a match term.
- The paired logit fits one coefficient vector to all respondents; it is not a mixed logit.
- Paired power requires a model from `utility_model()`; its coefficients are not calibrated to
  requested AMCEs. To request AMCEs directly, use `true_amce`, which runs AMCE regression only.

## Technical appendix

**Model.** Respondent *i* chooses profile 1 in task *t* with probability
logit⁻¹(*U*<sub>*it*1</sub> − *U*<sub>*it*2</sub>), where *U*<sub>*itj*</sub> = Σ<sub>*k*</sub>
*β*<sub>*ik*</sub> *x*<sub>*itjk*</sub> sums, over terms *k*, the respondent's coefficient times
the profile's score on that term: a dummy, a fixed score, a match indicator or the respondent's
rating. Coefficients and respondent scores are fixed across a respondent's tasks. Each task has two
profiles with complementary outcomes.

**Estimator.** `estimate_paired()` fits a logit without intercept to the profile-1-minus-profile-2
differences of the scores. Positive coefficients favour profile 1. Standard errors are clustered by
respondent: CR1 by default, CR2 (via `clubSandwich`) on the latent scale only. The probability scale
projects the fitted choice probabilities onto the ordinary AMCE regression; its standard errors
include the uncertainty of the fitted coefficients and of the sampled profiles and respondents. With
dummy terms for every attribute and nothing else, the probability estimates and their CR1 standard
errors coincide with ordinary AMCE regression.

**Targets.** With fixed coefficients and an analysis that uses the generating terms with the same
coding, the latent target equals the input coefficients. Otherwise it solves the population logit
score equations, exactly when all SDs are zero and the design is small enough to enumerate, and by
simulation otherwise (`paired_cells` per group per batch, 16 batches; the default is 2,097,152).
`reference_half_width` is the half-width of a confidence interval for a simulated target.
Probability-scale targets are the true population AMCEs, so a misspecified paired analysis shows its
bias. A target is marked `null = TRUE` only when a symmetry of the inputs proves it is zero, for
example identical inputs for a level and its reference, exchangeable levels, or coefficients shared
by two groups. A warning about an unresolved sign means a target is too close to zero, relative to
its precision, for Type S and Type M to be meaningful.

**Custom analyses.** `paired_terms` in `power_sim()` and `terms` in `estimate_paired()` select which
terms enter the paired logit: an attribute name selects its dummies, a score column name such as
`policy_rating` selects that term. Probability output keeps rows for every attribute, including ones
left out of the analysis, flagged `in_analysis = FALSE`; their targets are still the full population
AMCEs. Targets for a custom analysis are cached in `result$model$analysis_truths`.

**Reproducibility and speed.** Each simulated experiment has its own random-number stream, so
results do not depend on `cores`, and the first runs are unchanged when `sim_runs` grows. Target
calculations use separate streams and do not change with `units`, `n_tasks`, `estimators` or `seed`.
Reuse `result$model` to keep cached targets. `control` accepts `seed`, `paired_cells`,
`exact_max_pairs`, `exact_max_rows`, `exact_max_cells`, `verification_pairs`, `verification_draws`
and `verification_batches`; see `?utility_model`.

## References

Graham, M. H., & Svolik, M. W. (2020). Democracy in America? Partisanship, polarization, and the
robustness of support for democracy in the United States. *American Political Science Review*,
114(2), 392–409. <https://doi.org/10.1017/S0003055420000052>

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis:
Understanding multidimensional choices via stated preference experiments. *Political Analysis*,
22(1), 1–30. <https://doi.org/10.1093/pan/mpt024>

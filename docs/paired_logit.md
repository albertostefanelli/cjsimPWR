# Paired logit

## Why a paired logit

The paired logit in `cjsimPWR` is the conditional logit of discrete choice (McFadden 1974; Train 2009) applied to tasks with two profiles.[^polsci] Compared with AMCE regression, it offers three useful features.

- It can easily include characteristics of the respondent that relate to the profile. A term can record whether a level is the respondent's own or how the respondent rates it, so the same level can be an advantage for one respondent and a disadvantage for another.
- When respondents' characteristics are included, it can pool information about a shared preference across respondent groups and, thus, increase precision. For instance, to assess whether respondents reward candidates from their own party, subgroup AMCE analysis estimates every party's AMCE within each group of partisans. A match term instead estimates one coefficient, the value of a candidate from the respondent's own party, from all respondents.
- Its coefficients do not have the design-imposed bounds of AMCEs, which are changes in choice probability. This is useful when ratios of coefficients are used to express trade-offs in the units of the corresponding terms.[^design]

[^design]: A common rescaling of all utility coefficients cancels in their ratio (Train 2009, sec. 2.5.2). A ratio of pooled coefficients is not generally the average of respondents' trade-offs; see [Heterogeneous coefficients](#heterogeneous-coefficients).

[^polsci]: In political science, Graham and Svolik (2020) use it to estimate how much policy proximity and partisan loyalty voters give up to elect a more democratic candidate.

For power analysis, you describe how respondents choose (e.g., their ratings, the shares matching each level, how coefficients vary and how groups differ), and the package derives the coefficients that each analysis targets.

## Design

`conjoint_design()` names the attributes and lists their levels. The first level of each attribute is the reference level that the other levels are compared with.

```r
library(cjsimPWR)
design <- conjoint_design(list(
  policy    = c("left", "centre", "right"),
  party     = c("A", "B", "C"),
  democracy = c("D+", "D-")))
```

`D+` is a candidate who respects democratic rules and `D-` one who does not.

## Utility terms

Each attribute gets a term that says what a profile receives for it. A profile's utility is the sum of what it receives from all terms, and in each task the profile with the higher utility is the more likely to be chosen.[^logit] `coef` is the weight of a term, what one unit of its score adds to utility. Zero means the attribute does not matter, and a negative value counts against the profile.

[^logit]: Profile 1 is chosen with probability `plogis(U1 - U2)`, the logistic function of the utility difference. This is the choice model of the [simulation model guide](simulation_model.md#choice-model), and the reason the analysis is called a paired logit.

| Term | Score of a profile |
| --- | --- |
| `term_dummy()` | 1 for the non-reference level shown, 0 otherwise; one `coef` per non-reference level |
| `term_score()` | A fixed number assigned to each level in `scores` |
| `term_match()` | 1 when the level is the respondent's own, 0 otherwise; `share` gives the proportion of respondents matching each level |
| `term_rating()` | The respondent's own rating of the level, drawn from a normal distribution with `mean` and `sd` |

A respondent keeps the same ratings, party and coefficients in every task. Terms always go inside `list()`, even a single one. Every attribute needs a term. For an attribute that respondents see but that does not affect their choices, use `term_dummy("x", coef = 0)`: it stays in the experiment and the analysis, its target is exactly zero, and `power_sim()` reports its Type I error rather than power. Group-specific values are shown under [Groups](#groups).

In the example below:

- `policy`: every respondent has their own rating of each position. Ratings of `left` average 6 with an `sd` of 2, so about two thirds of respondents rate it between 4 and 8; `centre` averages 5 and `right` 4. A profile adds 0.05 for every rating point.
- `party`: 40 percent of respondents belong to party A, 30 percent to B and 20 percent to C; the remaining 10 percent belong to none. A profile from the respondent's own party adds 0.4.
- `democracy`: a candidate who does not comply with democratic norms (`D-`) loses 0.3, compared with `D+`, the reference level.

```r
model <- utility_model(design, terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2), coef = 0.05),
  term_match("party", share = c(0.4, 0.3, 0.2), coef = 0.4),
  term_dummy("democracy", coef = -0.3)))
model
```

```text
Latent utility model (logit)
Attributes (reference level first): policy [left, centre, right]; party [A, B, C]; democracy [D+, D-]
Groups: ungrouped

Coefficient means and SDs:
          term   type attribute level latent_mean latent_sd
 policy_rating rating    policy  <NA>        0.05         0
   party_match  match     party  <NA>        0.40         0
     democracy  dummy democracy    D-       -0.30         0

Population targets:
 estimator  scale        type        target input true_value  null
    paired latent coefficient policy_rating  0.05       0.05 FALSE
    paired latent coefficient   party_match  0.40       0.40 FALSE
    paired latent coefficient democracy[D-] -0.30      -0.30 FALSE
See $truth for full targets and precision, $parameters for coefficients, and $diagnostics for numerical details.
```

In the population targets, `input` is the coefficient you supplied and `true_value` what the analysis recovers with unlimited respondents. They are equal here; they differ when coefficients vary across respondents (see [Heterogeneous coefficients](#heterogeneous-coefficients)).

## Simulating and estimating one experiment

`simulate_experiment()` draws one data set from the model: here 500 respondents with five tasks each and two profiles per task.

```r
set.seed(1)

data <- simulate_experiment(design,
  units = 500,
  n_tasks = 5,
  model = model)

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

Each row is one profile, and `y` is 1 for the chosen one. Respondent 1 rates `left` 6.67 and `right` −0.46 and belongs to party A. In their first task they traded democracy for partisan loyalty: they chose a `D-` candidate of their own party over a `D+` candidate of party B.

`estimate_paired()` fits the paired logit. By default it uses all the model's terms and reports their coefficients:

```r
fit <- estimate_paired(data, design)
print(fit[c("term", "estimate", "std.error")], digits = 3)
```

```text
           term estimate std.error
1 policy_rating   0.0287    0.0185
2   party_match   0.5483    0.0707
3     democracy  -0.3830    0.0604
```


## Power

`power_sim(model = )` simulates the experiment `sim_runs` times, fits the paired logit each time and reports the share of runs in which each coefficient is significant: its power. `estimators = "paired"` asks for the paired logit alone, on the latent scale.

```r
latent <- power_sim(model = model,
                    units = 1000,
                    n_tasks = 5,
                    seed = 1,
                    sim_runs = 200,
                    estimators = "paired")
latent
```

```text
Conjoint simulation: 200 runs; logit DGP; CR1 / normal inference.
Estimators: paired; scale: latent
 estimator scale  type        term          attribute level true_value Power         Type S        Type M        Coverage      Runs (failed)
 paired    latent coefficient policy_rating policy    <NA>   0.05      0.995 (0.005) 0.000 (0.000) 1.005 (0.016) 0.945 (0.016) 200 (0)
 paired    latent coefficient party_match   party     <NA>   0.40      1.000 (0.000) 0.000 (0.000) 0.995 (0.008) 0.940 (0.017) 200 (0)
 paired    latent coefficient democracy     democracy D-    -0.30      1.000 (0.000) 0.000 (0.000) 1.000 (0.009) 0.975 (0.011) 200 (0)
Monte Carlo standard errors in parentheses.
Measures are conditional on runs with valid inference.
```

As in an AMCE power analysis (`power_sim()` with `true_amce`), the result stores the inputs and the targets and reports one row per coefficient. The numbers in parentheses are Monte Carlo standard errors.

- `Power`: the share of runs in which the coefficient is significant. A power of 1.000 (0.000) means every run rejected, not that rejection is certain.
- `Type S`: the share of significant estimates with the wrong sign.
- `Type M`: how much significant estimates exaggerate the true value on average (1.0 means not at all, 1.5 means 50 percent too large).
- `Coverage`: how often the 95 percent confidence interval contained `true_value`; it should be near 0.95.


## Comparing with AMCE regression

`estimators = c("amce", "paired")` runs ordinary AMCE regression and the paired logit on the same simulated experiments. Both are then reported on the probability scale, so that they estimate the same AMCEs. `power_sim()` prints a message to say so, and another reminding you that the policy and party rows compare levels, not rating points or the match coefficient.

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
1 policy: centre    -0.012      0.170        0.930   0.012     0.004
2  policy: right    -0.024      0.480        0.985   0.012     0.006
3       party: B    -0.009      0.150        0.855   0.013     0.003
4       party: C    -0.019      0.335        1.000   0.013     0.004
5  democracy: D-    -0.073      1.000        1.000   0.009     0.009
```

AMCE regression detects the small party-B effect in 15 percent of runs, the paired logit in 86 percent. This is to be expected in this case as the paired logit uses each respondent's ratings and party match; AMCE regression does not.[^recode]

[^recode]: AMCE regression can be used for the same purpose if the attributes are recoded relative to the respondent before fitting. Subgroup AMCEs by the respondent's party also use this information without assuming one bonus for every party, but each group's estimates come from that group alone, so they are less precise.

## Sample size

Hold the model and `n_tasks` fixed and compare values of `units`. This loop reports power for the `policy_rating` coefficient, the hardest of the three to detect, at 250 and 500 respondents.

```r
sizes <- c(250, 500)
rows <- vector("list", length(sizes))
for (i in seq_along(sizes)) {
  result <- power_sim(model = model, units = sizes[i], n_tasks = 5,
                      seed = 1, sim_runs = 200, estimators = "paired")
  policy <- subset(as.data.frame(result$performance), term == "policy_rating")
  rows[[i]] <- data.frame(units = sizes[i], power = policy$power, power_mcse = policy$power_mcse)
}
print(do.call(rbind, rows), digits = 2)
```

```text
  units power power_mcse
1   250  0.60      0.035
2   500  0.84      0.026
```

## Heterogeneous coefficients

`coef_sd` lets a coefficient vary across respondents. With `coef = -0.3, coef_sd = 0.3`, each respondent's democracy coefficient is drawn from a normal distribution with mean −0.3 and SD 0.3, so about two thirds of respondents lie between −0.6 and 0.

With varying coefficients, the population targets are computed by simulation, which takes minutes at the default precision. `control = list(paired_cells = 131072)` lowers the precision to finish in about ten seconds; use it to explore and the default for final numbers.

```r
mixed <- utility_model(design, terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2), coef = 0.05, coef_sd = 0.03),
  term_match("party", share = c(0.4, 0.3, 0.2), coef = 0.4),
  term_dummy("democracy", coef = -0.3, coef_sd = 0.3)),
  control = list(paired_cells = 131072))
print(mixed$truth[c("term", "input", "true_value", "reference_half_width")], digits = 3)
```

```text
           term input true_value reference_half_width
1 policy_rating  0.05     0.0493             0.000257
2   party_match  0.40     0.3953             0.000784
3     democracy -0.30    -0.2936             0.001154
```

`true_value` now differs from `input`. The paired logit fits one coefficient to everyone's choices even if each respondent has their own coefficient. Choice probabilities flatten out as they approach 0 or 1. So respondents above the average coefficient gain less probability than respondents below it lose. The population as a whole then looks less responsive than the average respondent, and the single fitted coefficient comes out closer to zero. The party target moves too, although its coefficient is fixed, because the varying policy and democracy coefficients add noise to every choice.

When `coef_sd` is used, you can create such a model once and reuse it. Its targets do not depend on `units`, `n_tasks` or the experiment `seed`.

## Groups

`groups` names respondent groups. Inputs that differ between groups are given as lists named by group; the others are shared. Here strong partisans gain 0.6 from a candidate of their own party and weak partisans 0.2; every respondent belongs to a party, and the ratings and the other coefficients are the same in both groups.

```r
partisans <- utility_model(design, groups = c("weak", "strong"), terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2), coef = 0.05),
  term_match("party", share = c(0.4, 0.35, 0.25), coef = list(weak = 0.2, strong = 0.6)),
  term_dummy("democracy", coef = -0.3)))
print(partisans$truth[c("group", "term", "true_value", "null")], digits = 3)
```

```text
          group          term true_value  null
1          weak policy_rating       0.05 FALSE
2          weak   party_match       0.20 FALSE
3          weak     democracy      -0.30 FALSE
4        strong policy_rating       0.05 FALSE
5        strong   party_match       0.60 FALSE
6        strong     democracy      -0.30 FALSE
7 strong - weak policy_rating       0.00  TRUE
8 strong - weak   party_match       0.40 FALSE
9 strong - weak     democracy       0.00  TRUE
```

The `strong - weak` rows are the differences between groups, which `power_sim()` also tests. The `party_match` difference, 0.4, is the extra reward strong partisans give a candidate of their own party, estimated from the candidates of all three parties at once. Two rows are marked `null = TRUE`: the package has proved from the inputs that they are exactly zero, because the groups share these coefficients. Such rows report `type_1_error`, the share of runs that wrongly find a difference, instead of power. `units` and `n_tasks` can be given per group: `units = c(weak = 300, strong = 300)`.

## Technical appendix

**Model.** Respondent $i$ chooses profile 1 in task $t$ with probability

$$
\Pr(Y_{it1} = 1) = \frac{1}{1 + e^{-(U_{it1} - U_{it2})}},
\qquad
U_{itj} = \sum_k \beta_{ik} x_{itjk}.
$$

Utility $U_{itj}$ sums, over terms $k$, the respondent's coefficient $\beta_{ik}$ times the profile's score $x_{itjk}$ on that term: a dummy, a fixed score, a match indicator or the respondent's rating. Coefficients and respondent scores are fixed across a respondent's tasks.

**Estimator.** `estimate_paired()` fits a logit without intercept[^intercept] to the profile-1-minus-profile-2 differences of the scores. Positive coefficients favour profile 1. Standard errors are clustered by respondent: CR1 by default, CR2 (via `clubSandwich`) on the latent scale only. The probability scale projects the fitted choice probabilities onto the ordinary AMCE regression. With dummy terms for every attribute and nothing else, the probability estimates and their CR1 standard errors coincide with ordinary AMCE regression.

[^intercept]: Choice probabilities depend only on utility differences (Train 2009, sec. 2.5.1). When attributes are randomized, position is the only systematic difference between the two profiles, so the intercept is close to zero (odds of about 1) unless respondents favour a position, for example by choosing the profile shown first more often. Randomly swapping which profile is coded as profile 1 in each task, independently of where it was shown, removes this bias.

**Targets.** A target is the value an analysis would recover with unlimited respondents; bias and coverage are measured against it. On the latent scale, the target equals the input coefficient unless:

1. The analysis leaves out a generating term (see Custom analyses). Its utility joins the unobserved part, and logit coefficients are measured relative to that, so the remaining coefficients shrink towards zero by about the same factor (Train 2009, sec. 2.5.2).
2. The analysis codes a term differently, for example party dummies when choices follow `term_match("party")`. The dummies then measure how many respondents belong to each party, not the own-party bonus.
3. A coefficient varies across respondents (`coef_sd` above zero).

In these cases the package solves the population logit score equations. It solves them exactly when no rating or coefficient varies across respondents (every `sd` and `coef_sd` is zero) and the design has few enough profile pairs to list them all. Otherwise it simulates them (`paired_cells` per group per batch, 16 batches; the default is 2,097,152) and reports `reference_half_width`, the half-width of a confidence interval for the target. A simulated target is itself an estimate, so the half-width shows whether its shift from the input is real, how small a bias can be trusted, and whether its sign is known.[^halfwidth]

[^halfwidth]: The interval is a 99 percent interval from the spread of the 16 batches, Bonferroni-adjusted for the number of targets; it is zero for exact targets. A bias smaller than the half-width may come from the target alone. `power_sim()` also reports `coverage_reference_lower` and `coverage_reference_upper`, the lowest and highest coverage for any target within the interval; if they differ noticeably, increase `paired_cells`.

Probability-scale targets are always the true population AMCEs, whatever the analysis, so a misspecified paired analysis shows up as bias on that scale.

A target is marked `null = TRUE` only when a symmetry of the inputs proves it is zero, i.e., identical inputs for a level and its reference, exchangeable levels, or coefficients shared by two groups. A warning about an unresolved sign means a target is closer to zero than its half-width, so Type S and Type M have no fixed reference.

**Custom analyses.** `paired_terms` in `power_sim()` and `terms` in `estimate_paired()` select which terms enter the paired logit: an attribute name selects its dummies, a score column name such as `policy_rating` selects that term. With the guide's model:

- Leave out a term, for example the policy ratings. The remaining coefficients shrink towards zero (case 1 under Targets).
- Experimental effects only, for example party dummies instead of the match (case 2 under Targets).
- Experimental effects plus a respondent characteristic, for example party dummies plus the match with the respondent's own party. Each party gets one value for everyone, and the own-party bonus comes on top.

**Reproducibility and speed.** Each simulated experiment has its own random-number stream, so results do not depend on `cores`, and the first runs are unchanged when `sim_runs` grows. Target calculations use separate streams and do not change with `units`, `n_tasks`, `estimators` or `seed`. Reuse `result$model` to keep cached targets. `control` accepts `seed`, `paired_cells`, `exact_max_pairs`, `exact_max_rows`, `exact_max_cells`, `verification_pairs`, `verification_draws` and `verification_batches`; see `?utility_model`.

## References

Graham, M. H., & Svolik, M. W. (2020). Democracy in America? Partisanship, polarization, and the robustness of support for democracy in the United States. *American Political Science Review*, 114(2), 392–409. <https://doi.org/10.1017/S0003055420000052>

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis: Understanding multidimensional choices via stated preference experiments. *Political Analysis*, 22(1), 1–30. <https://doi.org/10.1093/pan/mpt024>

McFadden, D. (1974). Conditional logit analysis of qualitative choice behavior. In P. Zarembka (Ed.), *Frontiers in Econometrics* (pp. 105–142). Academic Press.

Train, K. E. (2009). *Discrete Choice Methods with Simulation* (2nd ed.). Cambridge University Press. <https://eml.berkeley.edu/books/choice2.html>

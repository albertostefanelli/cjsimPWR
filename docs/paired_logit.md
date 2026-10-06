# Power from latent utility coefficients

`utility_model()` specifies preferences on the logit scale. `estimate_paired()` fits a logit of
profile 1 minus profile 2 predictors, with no intercept and standard errors clustered by respondent.
`power_sim()` can evaluate this estimator and ordinary AMCE regression on the same experiments.
The calibrated `true_amce`/`sigma` route retains its existing behavior.

## Specify preferences

```r
library(cjsimPWR)
design <- conjoint_design(list(
  policy = c("left", "centre", "right"),
  party = c("A", "B", "C"), democracy = c("D+", "D-")))
model <- utility_model(design, terms = list(
  term_rating("policy", mean = c(6, 5, 4), sd = c(2, 1.5, 2),
              coef = .3, coef_sd = .1),
  term_match("party", share = c(.4, .3, .2), coef = .9),
  term_dummy("democracy", coef = -.8, coef_sd = .3)))
model$parameters
model$truth
```

The first level of each attribute is the reference. A positive coefficient favors profile 1 when
its score is higher. The four term constructors define these scores:

| Constructor | Score of a level | Number of coefficients |
| --- | --- | ---: |
| `term_dummy()` | Indicator for each non-reference level | Levels minus one |
| `term_score()` | A supplied fixed numeric score | One |
| `term_match()` | One for the respondent's matching level, zero otherwise | One |
| `term_rating()` | The respondent's rating of that level | One |

Coefficient deviations are independent normal draws within each group. Ratings are independent normal
draws across levels and terms, with the supplied population means and SDs; they are neither clipped nor
rounded. Matches are categorical: at most one level matches. The unused share is the probability of
matching none. Respondent variables and coefficients are independent within group, and persist across
all that respondent's tasks. Different groups can have different distributions.

Every attribute needs a utility term. Set all its coefficient means and SDs to zero to make it inactive.
Multiple terms on an attribute are allowed when their default analysis is identified. A fixed score
beside a complete dummy block is aliased and rejected. Random ratings beside dummies may be identified.
The model checks the population Gram matrix analytically, so a rare match does not fail identification
simply because a numerical reference missed it.

Numeric level vectors include the reference, except dummy coefficients, which cover non-reference levels.
Unnamed vectors follow design order; named vectors must contain every required label exactly once.
Only an unnamed scalar `sd` or `coef_sd` is repeated across levels. Other vectors are never recycled.
Any numeric argument can instead be a list named by exactly the groups. Non-dummy coefficients are
unnamed scalars. Values must be finite, SDs nonnegative, scores nonconstant and shares between zero and
one. A share sum above `1 + 1e-12` is an error; a smaller excess above one is normalized once and used
consistently in simulation and truths.

Default term names are the attribute for dummies and `attribute_score`, `attribute_match` or
`attribute_rating` otherwise. List names can override non-dummy names. Names must be unique, and score
column names cannot conflict with attributes or the reserved data columns. Labels may contain spaces
and punctuation; they are treated as values, never formula fragments.

## Inspect and estimate an experiment

```r
set.seed(1)
data <- simulate_experiment(design, units = 500, n_tasks = 5, model = model)
head(data[c("respondent", "task", "profile", "policy", "policy_rating", "party", "party_match")])
estimate_paired(data, design)
estimate_paired(data, design, terms = c("policy", "democracy"))
```

The added score columns reveal the scores of levels actually shown. They cannot reconstruct unseen
ratings or distinguish no match from matching an unseen level. No respondent-table attribute is added.
`attr(data, "dgp")` contains the prepared model.

An attribute in `terms` selects its dummy block; a score-column name selects that column. The standalone
estimator also accepts external profile data, including calibrated simulations. It pairs rows by
respondent and task identifiers, independently of row order. Each task must contain one profile 1 and
one profile 2, complementary binary outcomes and finite selected scores. Missing values are errors.

Grouped inference reports each group followed by each later group minus the first. Failures are kept
in the table, with diagnostics: a failed group invalidates only its own rows and contrasts using it.
An unverified finite fit is reported as `nonconvergence`, without claiming mathematical separation.
The attached `vcov` matrix is in output-row order, including covariance between contrasts sharing a group.

## Choose the target and reporting scale

With `scale = "latent"`, the target is the population coefficient vector of the selected paired logit,
often written theta-star. It solves the population score equations. Input utility means, mu, are stored
separately in `input` and `parameters`. Random coefficients or omitted/recoded predictors can change
theta-star; attenuation in an example is not a general componentwise rule.

With fixed coefficients and an analysis matching all utility columns and their coding, theta-star is
exactly mu, even when ratings and matches vary. Equivalent recoding changes coefficients: score
`c(0, 2)` with coefficient `.3` has dummy coefficient `.6`.

With `scale = "probability"`, fitted profile probabilities are projected onto the ordinary AMCE design.
Bias and coverage use the **true population AMCE**, including any misspecification bias. Applying
`plogis()` to a coefficient in isolation is not this transform. Under independent uniform assignment,
the projection has an AMCE interpretation; with other assignments it remains the stated regression
projection. A dummy-only paired analysis covering every attribute has the same AMCE point estimates
and CR1 covariance as ordinary AMCE regression whenever both fits are finite and estimable.

CR1 supports both scales. Probability covariance includes uncertainty from both the fitted logit and
sampled profiles/respondents. CR2 supports latent paired estimates and ordinary AMCEs, and requires the
existing optional `clubSandwich` package. Paired probability CR2 is unavailable. Inference can be normal,
`t` with all-sample respondent count minus one degrees of freedom, or CR2 Satterthwaite. Full-sample
corrections retain counts from failed groups; a contributing group still needs at least two respondents.

## Compare power analyses

```r
# 200 runs illustrate the interface; use 1,000 or more for an actual analysis.
latent <- power_sim(model = model, units = 1000, n_tasks = 5,
                    seed = 1, sim_runs = 200, estimators = "paired")
both <- power_sim(model = model, units = 1000, n_tasks = 5,
                  seed = 1, sim_runs = 200, estimators = c("amce", "paired"))
both$performance
```

On the test machine described below, the two 200-run power calls together took 13.1 seconds,
including completion of the model's AMCE truth cache. Preparing the heterogeneous model in the first
example took 414.1 seconds. These are measured guide-run times during concurrent validation work;
prepared-model reuse avoids repeating that initial truth calculation.

Selecting both estimators chooses probability scale with a message; explicit latent scale is an error.
Paired-only power defaults to latent scale. AMCE-only power accepts either prepared model type; paired
power requires a latent model. Omit model-building arguments when supplying `model`. Power analyses can
select custom `paired_terms` from attributes and score columns generated by that model. Their latent
truths belong to the power result; the prepared model retains its default paired truth cache.

Each run simulates once, then fits each requested estimator separately. A paired failure leaves valid
AMCE results available. Power, Type I error, coverage, bias, empirical Type S/M with MCSEs, and analytic
Type S/M use the same `summarise_runs()` calculations on either scale. Analytic Type S/M are a known-SE
normal benchmark without MCSEs. Both probability estimators share targets and units. See the
[error-rate guide](error_rates.md) for interpretation. All measures condition on valid inference;
failed inference is never counted as nonsignificant.

## Groups, opposite ratings and certified nulls

```r
opposite_design <- conjoint_design(list(policy = c("left", "right"),
                                       democracy = c("D+", "D-")))
opposite <- utility_model(opposite_design, groups = c("A", "B"), amce = TRUE,
  terms = list(
    term_rating("policy", mean = list(A = c(0, 1), B = c(1, 0)), sd = 0, coef = 1),
    term_dummy("democracy", coef = -.8)))
opposite$truth
```

Both groups have the same rating coefficient of one and democracy coefficient of minus 0.8. The policy
AMCEs have opposite signs because the groups rate the positions differently. Their rating-coefficient
difference is an exact null; their policy-AMCE difference is not.

Nulls are certified by sufficient symmetry or analytic equality rules, never by a small numerical
estimate or an interval containing zero. Certified nulls report exact zero targets and zero uncertainty
for their means. They retain respondent-level AMCE SDs and their uncertainty: exchangeable random policy
ratings can produce zero policy AMCE and positive individual-effect SD. Certified nulls report Type I
error, and Type S/M are undefined. `null = FALSE` means no zero was certified.

Numerically computed paired truths preserve `raw_coefficients`, `raw_reference_vcov`, scaled scores
and Newton corrections in diagnostics. Reported certified zeros need not solve the finite-reference
score equations; acceptance checks and precision calculations use the unmodified raw solution.
`sign_resolved` is missing for certified nulls, true for analytic nonzero targets, and otherwise compares
the target with its own reference uncertainty, solver correction and roundoff guard. An unresolved sign
warns but does not suppress Type S/M. Increasing the integration budget addresses reference uncertainty;
solver or scaling uncertainty calls for inspection of the numerical diagnostics.

## Integration precision and reproducibility

Truths depend on the model and its integration controls, never the experiment's seed, respondent count
or task count. Exact finite-support integration is used when profile and joint match-cell limits allow
it. Otherwise, paired truth uses independent respondent/profile-pair cells, replayed in bounded blocks
for a shared damped Newton solve. AMCE integration uses separate antithetic respondent nodes and the
existing forward AMCE transform. Budgets are fixed: there is no acceptance-based reference resampling.

`control` accepts `seed`, `exact_max_pairs`, `exact_max_rows`, `exact_max_cells`, `paired_cells`,
`verification_pairs`, `verification_draws` and `verification_batches`. Inspect `model$control` for the
resolved values. `paired_cells` is per group per batch, independent of the AMCE respondent/profile
budgets. A sampled-reference rank failure asks for a larger budget, separately from population
identification. Unresolved numerical truths raise an error and are never returned as a last iterate.

The fixed default is **2,097,152 paired cells per group per batch**, with 16 batches. It was the first
of the five planned candidate budgets to meet a 0.001 standardized latent half-width target in all
seven development scenarios. All seven also passed the capped check at 4,194,304 cells: every change
in a raw coefficient was smaller than the sum of its two reference half-widths. This checks agreement
across budgets; it is not a proof of negligible integration bias for every model.

The benchmark ran on an Apple M1 Pro with eight logical CPUs, R 4.2.1 and OpenBLAS 0.3.29. These are
measured total times for paired truths alone, including generation and all solver passes, while
independent scenarios ran concurrently. Actual times depend on hardware and model complexity.

| Scenario | Largest standardized half-width | Construction seconds |
| --- | ---: | ---: |
| Guide model | 0.000267 | 370.4 |
| Weak coefficient heterogeneity | 0.000055 | 227.7 |
| Two-attribute heterogeneous rating model | 0.000197 | 124.1 |
| Rare party matches | 0.000248 | 218.5 |
| Omitted party analysis | 0.000341 | 207.9 |
| Two groups with different rating distributions | 0.000689 | 438.8 |
| Five-attribute model | 0.000325 | 380.5 |

Analytic matching-model truths and small finite-support models are much faster. Reuse a prepared model
across power analyses to reuse its cached truths. AMCE integration has its own budgets and precision;
the standardized latent target is not an AMCE tolerance.

Reference MCSEs and nominal simultaneous 99% batch-t half-widths use separate families for each paired
latent analysis and for AMCE means/SDs across groups. Distinct base-group quantities count once;
shared-reference copies and contrasts add no comparisons. Probability estimators reuse the same AMCE
family. Adding AMCE truths or another analysis leaves existing paired precision and sign flags unchanged.
These intervals quantify Monte Carlo error, not solver error or integration bias. Larger-budget stability
checks supplement them; increasing experiment runs does not improve truth precision.

Group roots use separate L'Ecuyer streams. Within each root, paired batch b uses substream `2*b - 1`
and AMCE batch b uses `2*b`; experiment streams keep substream zero. Allocation is unchanged by analysis
choices, analytic shortcuts or caches, including equal truth and experiment seeds. Identical group
specifications share their first occurrence's truth and reference stream. Truth calculations and
`power_sim()` restore the caller's RNG, including Box-Muller's cached state; `simulate_experiment()`
advances it. Experiment results agree across worker counts and preserve earlier runs when run count grows.

## LA-style analysis and limits

The democracy dummy and policy rating example reproduces the structure of a logit of `cc_vote_1` on
`cc_dminus` and `cc_policy1_rating`: predictors are focal-minus-opponent differences. Attribute assignment
here remains independent and uniform, with probability `1/L` at each level. LA assignment restrictions
and candidate relabeling are outside this model.

Derived coefficient ratios, equal-rating democracy penalties and trade-off power remain pending.
Under heterogeneity a fitted coefficient ratio is not automatically the mean individual's trade-off.
Other exclusions are paired power for calibrated models, pooled-group analyses, mixed-logit estimation,
calibration to AMCE targets, correlated coefficients or respondent variables, observed respondent tables,
bounded ratings, multiple simultaneous matches, survey weights, continuous randomized attributes and
production quadrature. Latent `coef_sd` can vary by term and group; this does not extend the calibrated
AMCE-scale `sigma` interface.

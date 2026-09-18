# To do

## Requested features

### 1. Rating outcomes

Many conjoints ask respondents to rate each profile, for example on a 1–7 scale, instead of or as well as
choosing between profiles. Simulate ratings from the same utility model, estimate AMCEs on the rating
scale with respondent-clustered standard errors, and calibrate so that requested effects are in rating
units. Respondents also differ in how they use the scale (some rate everything high), which adds
within-respondent correlation and should be simulated alongside effect heterogeneity.

### 2. Interactions

Power for interactions between attributes (AMCIEs, for example candidate gender × party) and for
conditional AMCEs. The model needs requested interaction effects, calibrated like the AMCEs, and the
analysis needs interaction terms in the regression. Interactions typically need far larger samples than
AMCEs (Schuessler and Freitag 2020), which is where simulation adds most; `cjpowR::cjpowr_amcie()` gives
a closed-form benchmark.

### 3. Heterogeneity specified by group, and by attribute

One `sigma` currently applies to every effect and every group. Allow a named `sigma` per group, for
example more disagreement among independents than among partisans; calibration already runs separately
for each group, so this is a small change. Extensions: `sigma` per attribute (divisive versus consensual
attributes), and correlated deviations across attributes for respondents whose preferences move together.

### 4. Trade-off analysis (Graham and Svolik 2020)

Graham and Svolik use a candidate-choice conjoint to infer how willing voters are to trade democratic
principles for other important candidate attributes. Their quantities differ from standard AMCEs and
are similar to marginal rates of substitution (MRS).

## Suggested additions

### 5. Sample-size planning

Power curves over the number of respondents and tasks, and a search for the smallest design that
reaches a target power for all effects of interest. This should make the respondents-versus-tasks
trade-off explicit: with heterogeneous preferences, extra tasks help less than extra respondents.

### 6. Marginal means and multiple testing

Report marginal means, which avoid the reference-level dependence of subgroup AMCE differences (Leeper,
Hobolt and Tilley 2020). Add joint power (all effects of interest significant in the same experiment) and
optional multiplicity corrections, such as Holm or the false discovery rate.

### 7. Richer designs

More than two profiles per task, opt-out options, restricted or weighted randomization (for example
excluding implausible combinations; de la Cuesta, Egami and Imai 2022), unequal numbers of tasks,
attrition, and task-order or fatigue effects. Each is currently an assumption of the
[simulation model](simulation_model.md#what-the-model-assumes).

### 8. Higher-level clusters

Sampling of countries, cities or other units, with clustering at that level. Such designs currently need
an external calculation (see [clustering](clustering.md)).

### 9. Faster calibration

Designs with sampled profiles and `sigma > 0` can take many minutes to calibrate (about 13 minutes in one
benchmark). Options include quasi-Monte Carlo integration, caching calibrated models, and common random
numbers across groups.

## Housekeeping

- **`reference_margin`.** Decide whether to keep, promote or remove the experimental option; on larger
  designs it multiplied calibration time by five to seven.
- **`dgp = "odds"`.** Remove the deprecated model of versions up to 0.2.1.

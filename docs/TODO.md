# To do

Proposals below are not commitments and carry no promised date; "needs design" marks one with an open
question beyond scope, not a lower priority than "proposed."

## General

| # | Proposal | User need | Current limitation | Status |
| --: | --- | --- | --- | --- |
| 1 | Rating outcomes | Analyse rating-scale conjoints (e.g. 1–7), not only forced choice. | Only paired forced choice is simulated; respondents' differing scale use is not modelled. | Proposed: simulate ratings from the same utility model, estimate on the rating scale, and add within-respondent scale-use correlation alongside effect heterogeneity. |
| 2 | Interactions | Power for AMCIEs (e.g. candidate gender × party) and conditional AMCEs. | The model has no requested interaction effects, and the analysis has no interaction terms. | Proposed: calibrate requested interactions like AMCEs and add interaction terms to the regression. Interactions typically need far larger samples than AMCEs (Schuessler and Freitag 2020); `cjpowR::cjpowr_amcie()` gives a closed-form benchmark. |
| 3a | AMCE heterogeneity by group | More disagreement in some subgroups than others (e.g. independents vs. partisans). | Calibrated AMCE-scale `sigma` is still shared across groups. | Proposed: named, group-specific calibrated AMCE-scale `sigma`. |
| 3b | AMCE heterogeneity by attribute | Divisive versus consensual attributes. | Calibrated AMCE-scale `sigma` is still shared across attributes. | Proposed: attribute-specific calibrated AMCE-scale `sigma`. |
| 4 | Sample-size planning | Find the smallest design reaching a target power for every effect of interest, not just evaluate one design. | The package answers "what is this design's power?", not "what design reaches a target?". | Proposed: power curves over `units` and `n_tasks`, and a search routine; should make the respondents-versus-tasks trade-off explicit, since extra tasks help less than extra respondents under heterogeneity. |
| 5 | Marginal means and multiple testing | Avoid the reference-level dependence of subgroup AMCE differences (Leeper, Hobolt and Tilley 2020); assess joint significance across several effects. | Only subgroup AMCEs and pairwise differences are reported; no marginal means or multiplicity correction. | Proposed: report marginal means; add joint power (all effects of interest significant in the same experiment) and optional corrections (e.g. Holm, false discovery rate). |
| 6 | Richer designs | More than two profiles per task, opt-out options, restricted or weighted randomization (e.g. excluding implausible combinations; de la Cuesta, Egami and Imai 2022), unequal task counts within groups, attrition, task-order or fatigue effects. | Each is currently a stated assumption of the [simulation model](simulation_model.md#assumptions-and-feasibility). | Needs design: broad scope; likely several separate decisions. |
| 7 | Higher-level clusters | Power and inference for designs sampling countries, cities or other units above the respondent. | The package clusters only by respondent (see [clustering](clustering.md)); such designs currently need an external calculation. | Needs design. |
| 8 | Faster calibration | Reduce wait time with heterogeneous coefficients. | Large calibration and population-reference calculations can still take many minutes. | Proposed: quasi-Monte Carlo integration and common random numbers across groups with different inputs. |

## Paired logit

These proposals extend the [paired-logit workflow](paired_logit.md); the interfaces below are illustrative.

1. **Simulate outcomes on supplied tasks.** Add a constructor such as `paired_design_from_data()`,
   usable by both `simulate_experiment()` and `power_sim()`. Preserve respondent IDs, task counts,
   policy ratings, democracy dimensions, issue identities and candidate assignments, then generate
   new votes from the specified utility model. An optional mode would sample whole respondents with
   all their tasks for sample-size planning, assigning a new respondent ID to each sampled copy so
   repeated draws remain separate clusters.

2. **Ratios of coefficients as a target.** Support trade-offs such as
   $MRS = -\beta_1 / \beta_2$ (Graham and Svolik 2020), currently computed by hand from each run.
   For example, `power_sim(..., ratios = list(MRS = c("democracy", "policy_rating")))` would define
   MRS as the negative of the first coefficient divided by the second. Report the mean and empirical
   SD of the run-wise estimates, their MCSEs, and confidence-interval coverage using the delta method
   or Fieller's method with the joint respondent-clustered coefficient covariance. Explicitly report
   undefined ratios and unbounded confidence sets when the denominator is zero or poorly determined.
   Under heterogeneity, the target is the negative ratio of the pooled analysis's two `true_value`s,
   not the ratio of the input coefficient means or the mean of individual ratios, which may not exist.

3. **Correlated or scale heterogeneity.** Extend the independent coefficient deviations drawn by
   `coef_sd` with a `coef_cor` correlation matrix. Also offer a one-parameter scale alternative:
   each respondent multiplies all coefficient means by one positive lognormal factor with mean 1,
   retaining that factor across tasks. For example, $s_i = \exp(\tau z_i - \tau^2 / 2)$ with
   $z_i \sim N(0,1)$ makes $\tau$ the sole scale-heterogeneity parameter and gives
   $\beta_{ik} = s_i\beta_k$. This varies preference strength while preserving each respondent's
   coefficient ratios. In the democracy calibration, independent coefficient draws reproduced the
   marginal SEs approximately but overstated the spread of MRS, motivating these alternatives.
   Covariance between coefficient estimates alone does not identify correlation between respondents'
   underlying preferences; recompute the fitted analysis's population targets under either extension.

## References

- de la Cuesta, B., Egami, N., & Imai, K. (2022). Improving the external validity of conjoint analysis:
  The essential role of profile distribution. *Political Analysis*, 30(1), 19–45.
  <https://doi.org/10.1017/pan.2020.40>
- Graham, M. H., & Svolik, M. W. (2020). Democracy in America? Partisanship, polarization, and the
  robustness of support for democracy in the United States. *American Political Science Review*, 114(2),
  392–409. <https://doi.org/10.1017/S0003055420000052>
- Leeper, T. J., Hobolt, S. B., & Tilley, J. (2020). Measuring subgroup preferences in conjoint
  experiments. *Political Analysis*, 28(2), 207–221. <https://doi.org/10.1017/pan.2019.30>
- Schuessler, J., & Freitag, M. (2020). Power analysis for conjoint experiments. SocArXiv.
  <https://doi.org/10.31235/osf.io/9yuhp>

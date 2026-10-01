# To do

Proposals below are not commitments and carry no promised date; "needs design" marks one with an open
question beyond scope, not a lower priority than "proposed."

| # | Proposal | User need | Current limitation | Status |
| --: | --- | --- | --- | --- |
| 1 | Rating outcomes | Analyse rating-scale conjoints (e.g. 1–7), not only forced choice. | Only paired forced choice is simulated; respondents' differing scale use is not modelled. | Proposed: simulate ratings from the same utility model, estimate on the rating scale, and add within-respondent scale-use correlation alongside effect heterogeneity. |
| 2 | Interactions | Power for AMCIEs (e.g. candidate gender × party) and conditional AMCEs. | The model has no requested interaction effects, and the analysis has no interaction terms. | Proposed: calibrate requested interactions like AMCEs and add interaction terms to the regression. Interactions typically need far larger samples than AMCEs (Schuessler and Freitag 2020); `cjpowR::cjpowr_amcie()` gives a closed-form benchmark. |
| 3a | Heterogeneity by group | More disagreement in some subgroups than others (e.g. independents vs. partisans). | One `sigma` applies to every group in a call. | Proposed: a named `sigma` per group; calibration already runs separately per group. |
| 3b | Heterogeneity by attribute | Divisive versus consensual attributes. | One `sigma` applies to every attribute. | Proposed: `sigma` per attribute. |
| 3c | Correlated heterogeneity | Respondents whose preferences across attributes move together. | Heterogeneity deviations are independent across attributes (see [assumptions](simulation_model.md#assumptions-and-feasibility)). | Proposed: correlated respondent-level deviations across attributes. |
| 4 | Trade-off analysis (Graham and Svolik 2020) | Estimate how large an improvement on one attribute respondents require to offset a cost on another — the rate at which they trade off attributes while holding overall utility fixed, analogous to a marginal rate of substitution (MRS) and distinct from an AMCE, which averages over attributes rather than holding utility constant. | Not simulated; needs its own requested quantities, since it is not an AMCE. | Needs design: define the estimand precisely before implementation. |
| 5 | Sample-size planning | Find the smallest design reaching a target power for every effect of interest, not just evaluate one design. | The package answers "what is this design's power?", not "what design reaches a target?". | Proposed: power curves over `units` and `n_tasks`, and a search routine; should make the respondents-versus-tasks trade-off explicit, since extra tasks help less than extra respondents under heterogeneity. |
| 6 | Marginal means and multiple testing | Avoid the reference-level dependence of subgroup AMCE differences (Leeper, Hobolt and Tilley 2020); assess joint significance across several effects. | Only subgroup AMCEs and pairwise differences are reported; no marginal means or multiplicity correction. | Proposed: report marginal means; add joint power (all effects of interest significant in the same experiment) and optional corrections (e.g. Holm, false discovery rate). |
| 7 | Richer designs | More than two profiles per task, opt-out options, restricted or weighted randomization (e.g. excluding implausible combinations; de la Cuesta, Egami and Imai 2022), unequal task counts, attrition, task-order or fatigue effects. | Each is currently a stated assumption of the [simulation model](simulation_model.md#assumptions-and-feasibility). | Needs design: broad scope; likely several separate decisions. |
| 8 | Higher-level clusters | Power and inference for designs sampling countries, cities or other units above the respondent. | The package clusters only by respondent (see [clustering](clustering.md)); such designs currently need an external calculation. | Needs design. |
| 9 | Faster calibration | Reduce wait time when `sigma > 0` and profiles must be sampled. | Calibration can take many minutes for such designs. | Proposed: quasi-Monte Carlo integration, caching calibrated models, common random numbers across groups. |

## Housekeeping

| Decision | Status |
| --- | --- |
| `reference_margin` | Undecided: keep, promote out of experimental status, or remove. |

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

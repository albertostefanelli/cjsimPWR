# DeclareDesign and cjsimPWR

`cjsimPWR` provides a dedicated workflow for power analysis of fully randomized, paired forced-choice
conjoint experiments. Such a simulation can also be assembled in
[DeclareDesign](https://declaredesign.org/r/declaredesign/) (Blair et al., 2019), a general framework in
which users construct designs and diagnosands from scratch:
[Declaration 17.5](https://book.declaredesign.org/library/experimental-descriptive.html) of Blair,
Coppock and Humphreys (2023) declares the choice model and the estimator, and takes the randomization,
the choices and the AMCEs from helper functions in the rdss package. Comparisons refer to DeclareDesign
1.1.1 and rdss 1.0.14.

| | `cjsimPWR` | DeclareDesign / Declaration 17.5 |
| --- | --- | --- |
| AMCE input | Direct: `true_amce` on the probability scale, calibrated to a set tolerance. | Through a utility model and rdss helper functions. |
| Workflow | One `power_sim()` call: generates, estimates and summarises performance — including subgroup AMCEs and their differences — for every effect at once. | Declare each design step (randomization, choice model, estimator, diagnosands) separately. |
| Flexibility | Fixed to this package's paired forced-choice conjoint model and diagnostics. | A general framework: can embed a conjoint in a larger design, or declare designs this package does not cover. |

Rating outcomes, where respondents score each profile instead of choosing between two, are not simulated
by `cjsimPWR`, and neither is clustering above the respondent, such as respondents sampled within
countries; DeclareDesign is a framework in which users can construct both. The remaining assumptions of
the simulation model are listed under
[assumptions and feasibility](simulation_model.md#assumptions-and-feasibility).

## References

Blair, G., Cooper, J., Coppock, A., & Humphreys, M. (2019). Declaring and diagnosing research designs.
*American Political Science Review*, 113(3), 838–859. <https://doi.org/10.1017/S0003055419000194>

Blair, G., Coppock, A., & Humphreys, M. (2023). *Research design in the social sciences: Declaration,
diagnosis, and redesign*. Princeton University Press.

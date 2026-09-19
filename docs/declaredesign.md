# DeclareDesign and cjsimPWR

`cjsimPWR` provides a dedicated workflow for power analysis of fully randomized, paired forced-choice
conjoint experiments. Such a simulation can also be assembled in
[DeclareDesign](https://declaredesign.org/r/declaredesign/) (Blair et al., 2019):
[Declaration 17.5](https://book.declaredesign.org/library/experimental-descriptive.html) of Blair,
Coppock and Humphreys (2023) declares the choice model and the estimator, and takes the randomization,
the choices and the AMCEs from helper functions in the rdss package. Compared with assembling it that
way, `cjsimPWR` offers:

- Direct AMCE specification. AMCEs are given on the probability scale through `true_amce`, not through a
  utility model. The default logit model is calibrated to reproduce them within a set tolerance (0.001 by
  default).
- A single-call workflow. `power_sim()` generates repeated experiments, estimates AMCEs and summarises
  performance without separately defining each simulation step.
- Interpretable preference heterogeneity. `sigma` is the standard deviation of respondent-level AMCEs,
  which allows sensitivity analysis with the average effects held fixed.
- Subgroup comparisons. Effects, sample sizes and task counts can differ by group. Subgroup AMCEs, and
  their differences from the first group, are estimated and tested directly.
- Diagnostics beyond power. Every AMCE and every difference between groups reports power, or Type I error
  when the target is zero, along with Type S and Type M errors (Gelman & Carlin, 2014), coverage, bias,
  and the empirical and model standard errors, each with its Monte Carlo standard error.
- Parallel simulation without setup. Supply a `seed` and set the number of workers through `cores`: no
  `future` plan is required, and the caller's random-number state is left unchanged.

In a local benchmark, generating and estimating one experiment took about 5 ms with `cjsimPWR` and
28 ms with the DeclareDesign example's generation and estimation steps (median of 40 repetitions;
500 respondents, 3 tasks each and attributes with 2, 2 and 4 levels). These timings exclude
`cjsimPWR`'s one-time calibration and the example's per-experiment AMCE truth calculations. Including
those truth calculations raised the DeclareDesign time to about 137 ms. The implementations use
different choice models, and timings depend on the design and machine.

Rating outcomes, where respondents score each profile instead of choosing between two, are not simulated,
and neither is clustering above the respondent, such as respondents sampled within countries.
DeclareDesign covers both, and can embed a conjoint in a larger design. The remaining assumptions of the
simulation model are listed in [what the model assumes](simulation_model.md#what-the-model-assumes).

Comparisons refer to DeclareDesign 1.1.1 and rdss 1.0.14.

## References

Blair, G., Cooper, J., Coppock, A., & Humphreys, M. (2019). Declaring and diagnosing research designs.
*American Political Science Review*, 113(3), 838–859. <https://doi.org/10.1017/S0003055419000194>

Blair, G., Coppock, A., & Humphreys, M. (2023). *Research design in the social sciences: Declaration,
diagnosis, and redesign*. Princeton University Press.

Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing Type S (sign) and Type M
(magnitude) errors. *Perspectives on Psychological Science*, 9(6), 641–651.

# The simulation model

`power_sim()` and `simulate_experiment()` generate forced-choice conjoint data whose population AMCEs
match the requested values within a calibration tolerance. This page explains the model, its
assumptions and its calibration; for the full calibration mechanics and precision controls, see the
[calibration guide](calibration.md).

## Choice model

Each respondent $i$ attaches a utility to every level of every attribute, and a profile's utility is
the sum of the utilities of the levels it shows. In task $t$, respondent $i$ sees profiles 1 and 2
and chooses profile 1 with probability

$$
\Pr(Y_{it1} = 1) = \frac{\exp(U_{it1})}{\exp(U_{it1}) + \exp(U_{it2})}
= \operatorname{logit}^{-1}\!\left(U_{it1} - U_{it2}\right),
\qquad
U_{itj} = \sum_{k=1}^{K} \beta_{ik}(x_{itjk}),
$$

where $x_{itjk}$ is the level of attribute $k$ shown in profile $j$, and $\beta_{ik}(\ell)$ is
respondent $i$'s utility for level $\ell$ of attribute $k$: a population value plus a Gaussian
deviation specific to the respondent (see [Heterogeneity](#heterogeneity-sigma)). Utilities are
measured relative to each attribute's reference level. Only the difference in utility between the two
profiles matters, and the two profiles of a task always receive complementary outcomes: exactly one is
chosen. Both profiles are drawn independently and uniformly over all attribute levels, as in the fully
randomized design of Hainmueller, Hopkins and Yamamoto (2014).

The AMCE of a level is the average change in the probability that a profile is chosen when that level
replaces the reference level, averaged over the other attributes, the competing profile and the
respondents. Because the choice model is nonlinear, the utility coefficients are not AMCEs, and (with
heterogeneity) an individual respondent's AMCE need not be normally distributed even though the
underlying utility deviations are Gaussian. You therefore request AMCEs (the `true_amce` argument), and
the package finds utilities that produce them (see [Calibration](#calibration) below):

```r
library(cjsimPWR)

design <- conjoint_design(c(2, 3, 5))
amce   <- list(0.05, c(-0.05, 0.05), c(-0.03, -0.05, -0.05, 0.05))
set.seed(1)
data   <- simulate_experiment(design, amce, units = 500, n_tasks = 5, sigma = 0.05)

attr(data, "dgp")$truth      # requested and true AMCEs, and the SD of respondent-level AMCEs
```

`dgp = "logit"`, the default, names this choice model. The argument is retained so that later versions
can add other choice models without changing existing calls.

## Assumptions and feasibility

Power is computed for data generated under these assumptions; departures in a real study change it.

- **Design:** paired forced choice only; ratings, opt-outs and larger choice sets are not simulated.
  Both profiles of every task are drawn independently and uniformly over attribute levels.
- **Stable choices:** given a respondent's preferences, tasks are independent, with no carryover,
  fatigue or order effects.
- **Sampling:** respondents are a simple random sample; higher-level clusters such as countries are not
  simulated (see [clustering](clustering.md)). Task counts are constant within a group, though they may
  differ between groups.
- **Effects:** utilities add up across attributes, and interactions between attributes cannot be
  requested. A level's effect on utility is therefore the same whatever the other attributes show,
  although its effect on the probability of choice is not.[^interaction]
- **Heterogeneity (`sigma`):** each respondent's deviations are drawn independently for each
  attribute, so a respondent who dislikes a level of one attribute is no more likely than anyone else
  to dislike a level of another.[^correlated]

[^interaction]: Utilities are on the logit (log-odds) scale, and the logistic curve that turns a
    utility difference into a choice probability is steep near 50 percent and flat near 0 and 100
    percent. The same utility gain therefore moves the probability more when the two profiles are
    otherwise evenly matched than when one is already far ahead. A gain of 0.4 raises the probability
    of choice from 0.50 to about 0.60 when the other attributes leave the profiles tied, but only from
    about 0.88 to 0.92 when they already give the profile a utility advantage of 2. On the probability
    scale, then, one attribute's AMCE depends on the levels of the others, even though no interaction
    was specified in the utilities. The package's AMCEs average over this, as the AMCE definition does.

[^correlated]: In real samples such preferences often move together: a conservative respondent may
    prefer both the older candidate and the one who opposes immigration. `groups` captures only part
    of this: it allows different AMCEs for named subgroups, such as Democrats and Republicans, but
    within each subgroup deviations remain independent across attributes.

Not every request is feasible. For an attribute with L levels, no AMCE can reach
`1 - 1/L` (0.5 for two levels), `AMCE^2 + sigma^2` must stay below `(1 - 1/L)^2`, and the marginal choice
probabilities implied by an attribute's AMCEs must lie within (0, 1). These are **necessary** conditions,
checked before solving, not a full feasibility proof: passing them does not guarantee a solution exists.
Separately, the iterative solver can fail to converge; that is numerical non-convergence, not a proof
that the requested population cannot exist (it may still indicate infeasibility or a boundary target).
Either failure stops the call with an error.

## Heterogeneity: `sigma`

`sigma` is a single non-negative number: the standard deviation, across respondents, of each
respondent's own AMCE, on the probability scale, shared across every effect and every group in one call.
`sigma = 0` gives every respondent the same AMCE. A level with a requested AMCE of zero has an average
effect of exactly zero in the population, even though individual
effects still vary around zero with SD `sigma`; Type I error refers to this population-average null.

The package draws Gaussian deviations of the utility coefficients, calibrated so that respondent-level
AMCEs have SD `sigma`. `latent_sigma` instead fixes the SD of the utility deviations directly and reports
the implied AMCE SDs (`amce_sd` in the `truth` table); only one of the two can be given.

There is no generally valid value of `sigma`; without pilot data, compare several values with the
requested AMCEs held fixed. `sigma` does not affect power with one task per respondent, but matters
more the more tasks each completes, because a respondent's deviation recurs in all of that
respondent's tasks.[^recur] See also the [closed-form guide](closed_form.md#sampling-variance) and
the [clustering guide](clustering.md#which-to-use).

[^recur]: Suppose the gender AMCE is 0.05 on average, but half the respondents prefer men (AMCE 0.20)
    and half prefer women (AMCE −0.10), so `sigma = 0.15`. A respondent who prefers men leans towards
    the man in every one of their tasks, so their ten choices repeat one preference rather than adding
    ten independent pieces of information. With 10 tasks per respondent, the variance of the
    estimated AMCE is about 41 percent larger than if the same 10 tasks came from 10 different
    respondents; with 50 tasks, more than three times as large. With a single task, there is nothing
    to repeat, and `sigma` has no effect.

```r
sigmas <- c(0, 0.05, 0.10, 0.15)
power  <- sapply(sigmas, function(s) power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500,
                                               n_tasks = 5, sigma = s, seed = 1)$performance$power)
colnames(power) <- sigmas
rownames(power) <- with(power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500, n_tasks = 5,
                                  sigma = 0, seed = 1)$performance, paste(attribute, level, sep = ":"))
power
```

## Calibration

You specify effects as AMCEs, on the probability scale, but the choice model works with utilities on
the logit scale, and no formula converts one into the other: the AMCE a utility produces depends on the
other attributes, the competing profile and how much respondents differ. Calibration closes this gap.
Once per group, the package searches for the logit-scale coefficients whose population AMCEs (and
respondent-level SD) match `true_amce` (and `sigma`), within 0.001 by default, and checks the result on
fresh draws. This takes seconds for small designs and minutes for large ones with heterogeneity; see
the [calibration guide](calibration.md) for details.

## Inspecting and reusing a model

The `truth` table (`attr(data, "dgp")$truth`, or `power_sim()`'s `$truth`) reports, for each effect and
group, the requested AMCE, the AMCE the generated population actually has (`true_amce`) and the SD of
respondent-level AMCEs (`amce_sd`); the [calibration guide](calibration.md#1-targets-and-numerical-truth)
explains the remaining columns.

The calibrated model depends on the design, the requested AMCEs, `sigma` and the groups, but not on the
number of respondents or tasks. Passing it back to `simulate_experiment()` skips calibration, the slow
step, and draws new respondents from exactly the same population. Three common uses:

- **Testing an analysis script.** Before fielding the study, run your planned estimation code on
  several simulated datasets and check that it recovers the AMCEs you set, without recalibrating for
  each dataset.
- **Mock data for a pre-analysis plan.** Generate a dataset with the structure of the real one, write
  and register the code, tables and figures on it, and run the same code on the real data later.
- **Comparing designs.** To decide, say, between 1,000 respondents with 3 tasks and 500 with 6, simulate
  both from the same model. Any difference then comes from the design alone, not from two calibrations
  that each hit the targets only within tolerance.

Reuse requires the same design and, with groups, the same group order as the prepared model:

```r
model <- attr(data, "dgp")   # or power_sim()'s $model
set.seed(2)
more  <- simulate_experiment(design, units = 1000, n_tasks = 3, model = model)
```

## References

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis:
Understanding multidimensional choices via stated preference experiments. *Political Analysis*,
22(1), 1–30. <https://doi.org/10.1093/pan/mpt024>

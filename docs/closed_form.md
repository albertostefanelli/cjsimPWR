# Closed-form power and simulation

Power for conjoint experiments can also be computed from the closed-form formulas of Schuessler and
Freitag (2020), implemented in their [cjpowR](https://github.com/m-freitag/cjpowR) package. The formulas
are instant, need no model of how respondents choose, and take the same AMCEs as cjsimPWR. This page
explains what they assume, when cjsimPWR gives the same answers, and when and why it does not.

## What the formulas compute

The formulas treat an AMCE as a comparison between two groups of profiles: those showing the level of
interest and those showing the reference level of the same attribute. With uniform randomization, each
group holds 1/L of the rated profiles, where L is the number of levels of the attribute. The standard
error is that of a difference between two proportions:

```
SE       = sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / (n / L))
p0       = 0.5 - AMCE / 2,   p1 = 0.5 + AMCE / 2
critical = qnorm(1 - alpha / 2)
rejection_probability = pnorm(AMCE / SE - critical) + pnorm(-AMCE / SE - critical)
```

This is the full two-sided normal expression used by
[cjpowR](https://github.com/m-freitag/cjpowR/blob/master/R/amce.R), with its default treatment
probability of 0.5. For a nonzero AMCE the rejection probability is power; at zero it is the theoretical
Type I error rate. Both tails matter: keeping only the first term would give 2.5%, not 5%, at zero
when `alpha = 0.05`, and would not handle negative AMCEs correctly.

Here n = respondents × tasks × 2 is the number of rated profiles, which Schuessler and Freitag call the
effective sample size. Three numbers go in: the AMCE, the number of levels of its attribute and n.
Nothing else about the study does: not the number of attributes, not the other attributes' effects, and
not whether the n profiles come from many respondents or from a few who complete many tasks.

Behind this simplification lie three assumptions:

1. **The effect in question is the only one that shapes respondents' choices.** Apart from it, every
   choice is treated as a coin flip.
2. **Every rated profile is an independent observation.** 500 respondents who complete 5 tasks count
   the same as 2,500 respondents who complete one.
3. **Type I error equals the significance level.** The formula treats its standard error as exact and
   uses normal critical values, so it assumes that a true null is rejected 5% of the time at a 5%
   significance level, and that a 95% confidence interval covers the true effect 95% of the time.

## A comparison

The table holds the AMCE of interest at 0.05 and the number of rated profiles at 2,000, and varies what
the formulas leave out. For every row, the formula gives a standard error of 0.0223 and a power of 0.61.
The simulated standard error is the average respondent-clustered standard error that the analysis
reports; in every row it matched the actual spread of the estimates to within 2%. Each row is based on
4,000 simulated experiments, so simulated power has a Monte Carlo standard error of about 0.008.

| Row | Design                                                  | Respondents × tasks | `sigma` | Standard error | Power |
| --: | :------------------------------------------------------ | :-----------------: | :-----: | -------------: | ----: |
|   1 | The attribute alone                                     |      1,000 × 1      |    0    |         0.0223 |  0.60 |
|   2 | The attribute alone                                     |       200 × 5       |    0    |         0.0223 |  0.61 |
|   3 | Plus three attributes whose AMCEs are all zero          |      1,000 × 1      |    0    |         0.0224 |  0.62 |
|   4 | Plus three attributes with AMCEs of 0.1 to 0.2 in size  |      1,000 × 1      |    0    |         0.0215 |  0.65 |
|   5 | The attribute alone                                     |      1,000 × 1      |  0.10   |         0.0223 |  0.60 |
|   6 | The attribute alone                                     |       200 × 5       |  0.10   |         0.0232 |  0.57 |
|   7 | The attribute alone                                     |      100 × 10       |  0.15   |         0.0265 |  0.48 |
|   8 | Plus three attributes with AMCEs of 0.1 to 0.2 in size  |       200 × 5       |  0.10   |         0.0224 |  0.62 |

Rows 1, 2, 3 and 5 agree with the formula, which is right to ignore the number of attributes (row 3),
several tasks per respondent when everyone has the same preferences (row 2), and differing preferences
when every respondent completes a single task (row 5). The other rows show what happens when its
simplifications do not hold. The code is at the end of this page.

## Each effect is computed as if it were the only one (1)

In a real conjoint, respondents weigh all attributes at once. A profile with several attractive features
tends to be chosen whichever level of the attribute of interest it shows, and one with several
unattractive features tends to be rejected, so many choices are far from a coin flip. The regression that
estimates the AMCEs includes every attribute and makes use of this: the more the other attributes
explain, the less unexplained noise is left around each AMCE.

Schuessler and Freitag set this aside on purpose. They analyse each attribute in a regression of its own,
which in large samples gives the same estimates as the full regression, and they place the choice
probabilities around 0.5 because that maximises the variance, which they describe as a conservative
choice. The formulas therefore give a worst case, and the full regression is more precise whenever the
other attributes have real effects. In row 4, three attributes with AMCEs of 0.1 to 0.2 reduce the
standard error by about 4% and raise power from 0.61 to 0.65. Adding attributes without effects changes
nothing (row 3), so what matters is the size of the other effects, not their number.

This error is on the safe side: on this count, the formulas can only understate power.

## Every rated profile counts as an independent observation (2)

The closed-form formulas count 5,000 profiles rated by 500 respondents the same as 5,000 profiles rated
by 2,500. This approximation works well when everyone has the same preferences (row 2) or when each
respondent completes a single task (row 5). It becomes too optimistic when respondents differ and
complete several tasks (rows 6 and 7). A respondent who cares a lot about an attribute tends to give it
similar weight in every task, so their choices partly repeat the same information. A sample that happens
to include more people who strongly favour (or oppose) a particular level therefore gives a larger (or
smaller) estimated effect for that level, and additional tasks per respondent cannot average this out.

Schuessler and Freitag discuss this point at length. They argue that clustering by respondent is
unnecessary when conclusions are meant only for the people who took part, because profiles, not people,
are randomized. For conclusions about the population the respondents were drawn from, the reason
Hainmueller, Hopkins and Yamamoto (2014) give for clustering, they accept that clustering is warranted
but argue that it makes little difference in practice. Row 6 shows a loss of the order they report;
row 7, however, shows that it can be much larger when respondents complete many tasks and disagree
strongly.

Preference heterogeneity is common in conjoint experiments (Robinson and Duch, 2024), and it is not
always captured by the characteristics used to define subgroups. For instance, respondents may be
unwilling to disclose socially unacceptable attitudes, such as prejudice against ethnic-minority
candidates. Robinson and Duch (2024) also show that heterogeneity persists within well-measured
ideological subgroups, and even among respondents at similar points on a 0–10 ideology scale.

Before fielding a study, researchers can therefore use pilot evidence to specify plausible
average effects and the variation around them, represented by `sigma`. Known subgroups with different
average effects should be specified separately; `sigma` then represents the remaining variation in
effects within each subgroup, including variation driven by attitudes that cannot be measured. Because
this remainder is rarely known precisely, compare several plausible values of `sigma`, for example
`sigma = c(0, 0.025, 0.05, 0.10, 0.15)`. With an average AMCE of 0.05, `sigma = 0.025` means
respondents agree on the direction and differ only in strength (about 2% have an effect of the opposite
sign).

## Type I error equals the significance level (3)

The closed-form formulas rely on the normal approximation and assume that the standard errors are right,
so that a true null is rejected 5% of the time and a 95% confidence interval covers the true effect 95%
of the time. With few respondents or small subgroups, both can fail: conventional respondent-clustered
standard errors can be too small, and normal critical values too permissive. These small-sample
problems motivate the corrections discussed by
[Pustejovsky and Tipton (2018)](https://doi.org/10.1080/07350015.2016.1247004).

cjsimPWR analyses each simulated experiment with the same estimator and standard errors that applied
studies use. It therefore measures the Type I error rate for true nulls and confidence interval
coverage instead of assuming them, and reports a Monte Carlo standard error for each figure. A
false-positive check is most useful in three situations:

- **Few respondents, especially with many tasks each.** Extra tasks add profile observations, not
  independent respondents, and clustered inference depends on the number of respondents: with few of
  them, standard errors tend to be too small and the test can reject too often. A calculation based on
  the total number of profiles cannot reveal this.
- **Small or unequal subgroups.** A large total sample can hide a small subgroup whose AMCEs and group
  differences rest on few respondents.
- **Comparing designs or analysis choices.** When one respondent/task allocation, heterogeneity
  scenario or inference method appears more powerful, a null check helps establish whether that
  advantage comes with an acceptable false-positive rate.

Note that agreement between simulation and the closed-form formula on power does not remove the need
for Type I error checks: agreement at a nonzero effect says nothing about how inference behaves when the
effect is zero.

### How to make the comparison

For an AMCE of interest, simulate the planned nonzero target to estimate power, then simulate again
with only that target set to zero. Keep the other effect targets, groups, sample sizes, tasks,
heterogeneity, significance level and analysis method unchanged. For example, compare an education
AMCE of 0.05 with an education AMCE of zero while keeping an experience AMCE of 0.10 in both scenarios.
The first run measures detection of an education effect; the second measures false alarms about
education when experience still matters. A zero average does not require setting heterogeneity to zero:
individual preferences can differ while cancelling on average.

## The errors can cancel

Violations of the first two assumptions err in opposite directions. Row 8 combines them: the other
attributes' effects make the estimate more precise, heterogeneity across five tasks makes it less
precise, and the resulting power of 0.62 is close to the formula's 0.61. The agreement is a
coincidence. With more tasks or more heterogeneity the formula would be too optimistic, and with larger
effects on the other attributes too pessimistic; only a simulation of the design shows which.

## Reproducing the comparison

```r
library(cjsimPWR)

# The formula; cjpowR::cjpowr_amce(amce = 0.05, n = 2000, levels = 2) gives the same power
formula_se <- function(amce, n, levels) {
  p0 <- 0.5 - amce / 2
  p1 <- 0.5 + amce / 2
  sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / (n / levels))
}
formula_rejection <- function(amce, n, levels, alpha = 0.05) {
  signal <- amce / formula_se(amce, n, levels)
  critical <- qnorm(1 - alpha / 2)
  pnorm(signal - critical) + pnorm(-signal - critical)
}
formula_se(0.05, n = 2000, levels = 2)         # 0.0223
formula_rejection(0.05, n = 2000, levels = 2)  # 0.61
formula_rejection(0, n = 2000, levels = 2)     # 0.05: theoretical null rate

# The table; each row is one power_sim() call. Add cores = 4, say, to run faster: results are
# identical for any number of workers.
others <- list(c(0.1, 0.2), c(-0.1, 0.1), c(0.1, 0.15, 0.2))
zeros  <- list(c(0, 0), c(0, 0), c(0, 0, 0))
rows <- list(
  list(levels = 2, true_amce = list(0.05), units = 1000, n_tasks = 1, sigma = 0),
  list(levels = 2, true_amce = list(0.05), units = 200, n_tasks = 5, sigma = 0),
  list(levels = c(2, 3, 3, 4), true_amce = c(list(0.05), zeros), units = 1000, n_tasks = 1, sigma = 0),
  list(levels = c(2, 3, 3, 4), true_amce = c(list(0.05), others), units = 1000, n_tasks = 1, sigma = 0),
  list(levels = 2, true_amce = list(0.05), units = 1000, n_tasks = 1, sigma = 0.10),
  list(levels = 2, true_amce = list(0.05), units = 200, n_tasks = 5, sigma = 0.10),
  list(levels = 2, true_amce = list(0.05), units = 100, n_tasks = 10, sigma = 0.15),
  list(levels = c(2, 3, 3, 4), true_amce = c(list(0.05), others), units = 200, n_tasks = 5, sigma = 0.10)
)
results <- lapply(rows, function(row) {
  do.call(power_sim, c(row, sim_runs = 4000, seed = 1))$performance[1, c("model_se", "emp_se", "power")]
})
do.call(rbind, results)

# Few respondents: Type I error for the attribute whose AMCE is zero
for (inference in c("normal", "t")) {
  few <- power_sim(levels = c(2, 2), true_amce = list(0.05, 0), units = 30, n_tasks = 10,
                   sigma = 0.10, inference = inference, sim_runs = 4000, seed = 1)
  print(few)  # the default output includes power and Type I error, each with its Monte Carlo SE
}
```

## References

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis:
Understanding multidimensional choices via stated preference experiments. *Political Analysis*,
22(1), 1–30.

Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for cluster-robust variance estimation and
hypothesis testing in fixed effects models. *Journal of Business & Economic Statistics*, 36(4), 672–683.
<https://doi.org/10.1080/07350015.2016.1247004>

Robinson, T. S., & Duch, R. M. (2024). How to detect heterogeneity in conjoint experiments. *The Journal
of Politics*, 86(2), 412–427. <https://doi.org/10.1086/727597>

Schuessler, J., & Freitag, M. (2020). Power analysis for conjoint experiments. SocArXiv.
<https://doi.org/10.31235/osf.io/9yuhp>

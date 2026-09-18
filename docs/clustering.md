# Clustering

The package default estimates AMCEs by ordinary least squares on profile rows with standard errors clustered by respondent. Clustering by respondent is always on. Two arguments control the rest:

- `vcov`: how the clustered covariance is computed, `"CR1"` (default) or `"CR2"`.
- `inference`: the critical values, `"normal"`, `"t"` or `"Satterthwaite"`. If omitted, CR1 uses
  `"normal"` and CR2 uses `"Satterthwaite"`.

The default, CR1 with normal critical values, gives the same standard errors, p-values and confidence
intervals as `cjoint::amce()` with `cluster = TRUE`.[^cr1] Neither choice changes the point estimates.
`vcov` changes the standard errors; switching from normal to t inference leaves them unchanged but
changes p-values and confidence intervals. Both choices can affect power, Type I error, coverage, Type S and Type M.

## Selecting a method

```r
library(cjsimPWR)

# A zero AMCE lets each method report Type I error as well as power
amce <- list(0.05, c(-0.05, 0), c(-0.03, -0.05, -0.05, 0.05))

# Default: CR1 covariance, normal critical values (as cjoint::amce())
power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500, n_tasks = 5, seed = 1)

# CR1 covariance, t critical values with G - 1 degrees of freedom (G = respondents in the fit)
power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500, n_tasks = 5, seed = 1,
          inference = "t")

# CR2 covariance, Satterthwaite degrees of freedom for each effect (needs clubSandwich)
power_sim(levels = c(2, 3, 5), true_amce = amce, units = 500, n_tasks = 5, seed = 1,
          vcov = "CR2", cores = 2)

# The same arguments apply to a single data set
design <- conjoint_design(c(2, 3, 5))
data <- simulate_experiment(design, amce, units = 500, n_tasks = 5)
estimate_amce(data, design, vcov = "CR2")
```

Compare power together with the Type I error shown for the zero effect: higher rejection rates can
reflect more false positives, not greater precision. At `alpha = 0.05`, aim for Type I error near 5%,
allowing for its Monte Carlo standard error. A check for one null effect does not validate every other
effect or subgroup; rerun with the effect of interest set to zero when needed.

## What the options do

- **CR1** is the usual cluster-robust sandwich estimator with the small-sample factor
  G/(G − 1) × (n − 1)/(n − k), where G is the number of respondents, n the number of profile rows
  and k the number of coefficients.[^cr1]
- **t** uses G − 1 degrees of freedom, where G counts all respondents in the fit, groups included.
  A small subgroup in a large sample therefore gets the large sample's degrees of freedom.
- **CR2** is the bias-reduced covariance of Bell and McCaffrey (2002), and **Satterthwaite** gives
  each effect its own degrees of freedom, driven by the respondents that inform it (Pustejovsky and
  Tipton 2018).[^cr2] For instance, with 1,000 respondents in one group and 30 in another, `inference = "t"` uses
  1,029 degrees of freedom for every effect; CR2/Satterthwaite computes degrees of freedom from the
  realized design for each AMCE and group difference. These can be much smaller and cannot be
  determined from group sizes alone.[^cost]

## Which to use

**Several tasks per respondent.** Use the default, as recommended by Hainmueller, Hopkins and
Yamamoto (2014).[^one] All choices of a respondent share that person's preferences, and randomizing the
attributes does not remove the uncertainty that comes from sampling respondents with different
effects. Clustering matters more the more tasks each respondent completes and the more preferences
vary (`sigma`);[^sigma] the number of attributes does not matter.

**Few respondents, many tasks.** Additional tasks estimate each sampled respondent's preferences
more precisely; they do not add respondents, and inference rests on the number of respondents. If
individual AMCEs vary across respondents with standard deviation `sigma`, the sampling variance of
their average is `sigma^2 / N`, where `N` is the number of respondents, regardless of the number of
tasks. With 100 respondents completing 30 tasks each, for example, the data contain 3,000 choices
but only 100 clusters. Where possible, recruit more respondents rather than adding tasks. When the
number of respondents is fixed, use `inference = "t"` at least, or `vcov = "CR2"`, and report the
degrees of freedom of each effect. No method guarantees accurate inference with very few
respondents.[^few]

**Small subgroups.** When a subgroup has few respondents, or its respondents contribute unevenly (for example,
different numbers of tasks per group), use `vcov = "CR2"`. A large total sample does not by itself ensure
reliable inference for a small subgroup's AMCE. A group difference uses respondents from both groups,
but the smaller group can limit its precision (Pustejovsky and Tipton 2018).

**Note on respondents from several countries, cities or other clusters.** The package clusters only by respondent and does not
simulate sampled clusters. Such designs need an external power calculation and analysis, because
clustering at a higher level is warranted only when sampling or treatment assignment happens at that
level (Abadie, Athey, Imbens and Wooldridge 2023). For instance, when 20 of 1,000 cities are sampled
and respondents are then sampled within them, the cities are the sampled units and standard errors must
be clustered by city.[^cities] This option is not available in the package, as such designs are rare in political-science conjoint experiments.

[^cr1]: The same numbers as Stata's `vce(cluster)`, `sandwich::vcovCL(type = "HC1")`,
    `clubSandwich::vcovCR(type = "CR1S")` (not its `"CR1"`), `estimatr::lm_robust(se_type = "stata")`
    and `cjoint::amce(cluster = TRUE)` for an unweighted model.

[^cr2]: The 2023 corrigendum to Pustejovsky and Tipton concerns a computational shortcut for
    fixed-effects models; it does not affect the unweighted OLS fit used here.

[^cost]: CR2 is slower: roughly 0.4 s per fit against 6 ms for CR1 in the design above, which is
    several minutes for the default 1,000 runs. Use `cores`.

[^one]: This also holds with one task per respondent. A task contributes two linked rows, one chosen
    and one rejected, so a respondent is one cluster even with a single task, and respondent and
    task clustering coincide. In a fully randomized design the clustered and unclustered standard
    errors are then almost equal, so clustering costs nothing and is kept on. Use `inference = "t"`
    when respondents are few.

[^sigma]: `sigma` is the standard deviation of respondent-level AMCEs within a group. Respondents
    split equally between individual effects of −0.02 and +0.08 have a mean AMCE of 0.03 with
    `sigma = 0.05`. There is no generally valid value; without pilot evidence, compare scenarios such
    as `sigma = c(0, 0.05, 0.10, 0.15)` with the requested AMCEs held fixed.

[^cities]: If an attribute's effect is 2 points in some cities and 10 in others, which cities are
    drawn changes the pooled effect even though attributes are perfectly randomized; Abadie et al.
    (2023, §3.1) derive this variance component.

[^few]: MacKinnon, Nielsen and Webb (2023) show that leverage and unequal cluster contributions
    matter as much as the number of clusters, and favour cluster-jackknife or wild-cluster-bootstrap
    inference in hard cases; these are not implemented in the package.

## References

Abadie, A., Athey, S., Imbens, G. W., & Wooldridge, J. M. (2023). When should you adjust standard
errors for clustering? *The Quarterly Journal of Economics*, 138(1), 1–35.
<https://doi.org/10.1093/qje/qjac038>

Bansak, K., Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2018). The number of choice tasks and
survey satisficing in conjoint experiments. *Political Analysis*, 26(1), 112–119.

Bell, R. M., & McCaffrey, D. F. (2002). Bias reduction in standard errors for linear regression with
multi-stage samples. *Survey Methodology*, 28(2), 169–181.

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis:
Understanding multidimensional choices via stated preference experiments. *Political Analysis*,
22(1), 1–30.

Leeper, T. J., Hobolt, S. B., & Tilley, J. (2020). Measuring subgroup preferences in conjoint
experiments. *Political Analysis*, 28(2), 207–221.

MacKinnon, J. G., Nielsen, M. Ø., & Webb, M. D. (2023). Cluster-robust inference: A guide to empirical
practice. *Journal of Econometrics*, 232(2), 272–299.

Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for cluster-robust variance estimation
and hypothesis testing in fixed effects models. *Journal of Business & Economic Statistics*, 36(4),
672–683. Corrigendum (2023): <https://doi.org/10.1080/07350015.2023.2174123>

Barari, S., Berwick, E., Hainmueller, J., Hopkins, D., Liu, S., Strezhnev, A., & Yamamoto, T. cjoint:
AMCE estimator for conjoint experiments. R package. <https://CRAN.R-project.org/package=cjoint>

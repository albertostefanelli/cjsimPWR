# Clustering

The package default estimates AMCEs by ordinary least squares on profile rows with standard errors
clustered by respondent. Clustering by respondent is always on; `vcov` (`"CR1"` or `"CR2"`) and
`inference` (critical values) control the rest. Neither choice changes the point estimates.

| Method | `vcov` | `inference` | Degrees of freedom | Dependency |
| --- | --- | --- | --- | --- |
| CR1 / normal (default) | `"CR1"` | `"normal"` (default for CR1) | `Inf` | Base R (`sandwich`). Matches `cjoint::amce(cluster = TRUE)` for the unweighted specification, and Stata's `vce(cluster)` (Barari et al.). |
| CR1 / t | `"CR1"` | `"t"` | `G - 1`, where `G` is every respondent in the joint fit, groups included | Base R (`sandwich`). The `G - 1` approximation can be poor for a small subgroup even when the joint fit has many respondents. |
| CR2 / Satterthwaite (default for CR2) | `"CR2"` | `"Satterthwaite"` (default for CR2; `"normal"`/`"t"` also accepted, but Satterthwaite itself requires CR2) | Per effect, from the respondents that inform it (Pustejovsky and Tipton 2018) | Requires the `clubSandwich` package. Bias-reduced (Bell and McCaffrey 2002). |

CR1 is the cluster-robust sandwich covariance with the small-sample factor
`G/(G-1) * (n-1)/(n-k)` (`G` respondents, `n` profile rows, `k` coefficients) — `sandwich::vcovCL(type = "HC1", cadjust = TRUE)`,
`clubSandwich`'s `"CR1S"` (not its differently named `"CR1"`), and `estimatr::lm_robust(se_type = "stata")`.
With 1,000 respondents in one group and 30 in another, `inference = "t"` uses 1,029 degrees of freedom for
every effect, while CR2/Satterthwaite computes degrees of freedom from the realized design for each AMCE
and group difference — these can be much smaller and are not determined by group sizes alone.

## Selecting a method

```r
library(cjsimPWR)

# A zero AMCE lets each method report Type I error as well as power
amce <- list(0.05, c(-0.05, 0), c(-0.03, -0.05, -0.05, 0.05))

common <- list(levels = c(2, 3, 5), 
	true_amce = amce, 
	units = 500, 
	n_tasks = 5, 
	seed = 1)

do.call(power_sim, common)                                          # default: CR1, normal
do.call(power_sim, c(common, list(inference = "t")))                # CR1, t (G - 1 df)
do.call(power_sim, c(common, list(vcov = "CR2")))                   # CR2, Satterthwaite (needs clubSandwich)
```

Compare power together with the Type I error shown for the zero effect: higher rejection rates can
reflect more false positives, not greater precision. At `alpha = 0.05`, aim for Type I error near 5%,
allowing for its Monte Carlo standard error. A check for one null effect does not validate every other
effect or subgroup; see the [error-rates guide](error_rates.md#checking-an-effect) for rerunning with
an effect of interest set to zero.

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

Bell, R. M., & McCaffrey, D. F. (2002). Bias reduction in standard errors for linear regression with
multi-stage samples. *Survey Methodology*, 28(2), 169–181.

Hainmueller, J., Hopkins, D. J., & Yamamoto, T. (2014). Causal inference in conjoint analysis:
Understanding multidimensional choices via stated preference experiments. *Political Analysis*,
22(1), 1–30. <https://doi.org/10.1093/pan/mpt024>

MacKinnon, J. G., Nielsen, M. Ø., & Webb, M. D. (2023). Cluster-robust inference: A guide to empirical
practice. *Journal of Econometrics*, 232(2), 272–299. <https://doi.org/10.1016/j.jeconom.2022.04.001>

Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for cluster-robust variance estimation
and hypothesis testing in fixed effects models. *Journal of Business & Economic Statistics*, 36(4),
672–683. <https://doi.org/10.1080/07350015.2016.1247004>. Corrigendum (2023):
<https://doi.org/10.1080/07350015.2023.2174123>

Barari, S., Berwick, E., Hainmueller, J., Hopkins, D., Liu, S., Strezhnev, A., & Yamamoto, T. cjoint:
AMCE estimator for conjoint experiments. R package. <https://CRAN.R-project.org/package=cjoint>

# Tradeoff analysis

## Why AMCEs alone do not generally identify tradeoffs

The average marginal component effect (AMCE) is the standard estimand in conjoint experiments. In paired-profile forced-choice designs, it represents the average change in a profile's probability of being chosen when one attribute changes, averaging over respondents, the profile's remaining attributes, and the attributes of the alternative profile ([Hainmueller, Hopkins, and Yamamoto 2014](https://doi.org/10.1093/pan/mpt024)). Recently, several scholars have exposed shortcomings in how AMCEs are commonly interpreted and clarified what they can and cannot be used for. Despite this, little has been written on whether the AMCE is a suitable estimand for assessing whether respondents make tradeoffs between experimental attributes, that is, whether a respondent who dislikes one attribute accepts it anyway to obtain something they value.[^def]

[^def]: The definition has two parts, and both are needed. The respondent must dislike one attribute, so that accepting it entails a sacrifice, and value the benefit of another attribute enough to make that sacrifice worthwhile. We must therefore establish explicitly or assume that all respondents dislike the drawback and value the benefit.

This distinction is not trivial and matters for interpreting substantive findings. In their conjoint on pension reforms, [Häusermann, Kurer, and Traber (2019)](https://doi.org/10.1177/0010414018797943), for example, find that pension cutbacks reduce support for reform packages, whereas expanding occupational-pension eligibility increases it. They interpret this pattern as evidence that citizens may accept cuts to existing benefits in exchange for occupational pensions. Yet establishing such a tradeoff requires more than a comparison of separate AMCEs. The following examples explain why.

### Averaging over respondents: heterogeneity

First, averaging across respondents can hide disagreement about an attribute. Consider a sample divided equally between Democrats and Republicans and a proposal to gerrymander electoral districts in a way that benefits Republicans. For this illustration, suppose that every Democratic respondent has an AMCE of −10 percentage points, as they expect their party to lose seats, and every Republican respondent has one of +10 percentage points, as they expect their party to gain seats. The pooled AMCE is

$$
\mathrm{AMCE}_{\text{gerrymandering}}
=
0.5(-0.10)+0.5(0.10)
=
0.
$$

This zero AMCE reflects cancellation between opposing average responses, even though gerrymandering strongly structures choices in both groups. Read as evidence about tradeoffs, the zero would say that gerrymandering costs nothing and that no tradeoff is needed, although every Democrat in the sample faces one whenever gerrymandering comes bundled with something they value.[^pooledlogit]

[^pooledlogit]: This issue is not specific to the AMCE. A logit model fitted to all respondents with a single coefficient per attribute averages opposing responses in the same way. The problem disappears only when heterogeneity is modeled, either by interacting the attributes with observed respondent characteristics such as party or by estimating respondent-specific effects, which requires either assumptions about how preferences are distributed across respondents ([Train 2009](https://doi.org/10.1017/CBO9780511805271), ch. 6) or many responses per respondent ([Zhirkov 2022](https://doi.org/10.1017/pan.2021.4)).

This is one instance of the aggregation problem examined by [Abramson, Koçak, and Magazinnik (2022)](https://doi.org/10.1111/ajps.12714), who show that the AMCE can have the opposite sign from the effects experienced by most respondents, because it combines magnitude as well as direction. To see the role of magnitude, now suppose instead that Republicans constitute 60% of the sample and every one of them supports gerrymandering by +10 percentage points, while Democrats constitute 40% and oppose it by −30 points. Then

$$
\mathrm{AMCE}_{\text{gerrymandering}}
=
0.6(0.10)+0.4(-0.30)
=
-0.06.
$$

Average support falls by 6 percentage points even though the majority rewards gerrymandering and has a positive AMCE.[^applied] For tradeoff analysis this matters because a tradeoff requires respondents to dislike something to accept something else they value. The pooled AMCE, however, does not reveal for whom, if anyone, the attribute is a cost.[^monotone] To see the consequence, add an attribute recording whether unemployment rose rather than fell during the candidate's previous term. Suppose every respondent penalizes a rise in unemployment by 15 points. Interpreted as a tradeoff, the pooled results, −6 points for gerrymandering and −15 for a bad record, suggest that voters would accept gerrymandering in exchange for a better economic record. Yet nobody in the electorate makes that tradeoff. The 60 percent who favor gerrymandering sacrifice nothing by accepting it. The remaining 40 percent penalize gerrymandering twice as strongly as a bad record and would accept the bad record to avoid it.[^electoral] In other words, comparing separate AMCEs commits an ecological fallacy: it combines marginal averages across attributes without identifying the joint distribution of preferences, describing a tradeoff that zero percent of the electorate is willing to make.

[^applied]: Applied research rarely acknowledges this issue, with the exception of [Wunsch, Jacob, and Derksen (2025)](https://doi.org/10.1017/S0007123424000711), who explain that negative AMCEs for democratic liberties can reflect either broadly shared opposition or a mixture of respondents who strongly punish violations, overlook them, or approve of them.

[^monotone]: The direction problem arises only when some respondents welcome what others reject and the analysis does not separate them. If gerrymandering were a cost for everyone, or a cost for some and a matter of indifference to the rest, the pooled AMCE would be the average cost in a population that uniformly dislikes it. This is a monotonicity assumption (see [Imbens and Angrist 1994](https://doi.org/10.2307/2951620)).

[^electoral]: It is important to note that in a forced-choice conjoint, the pooled AMCEs represent the overall vote share correctly. See [Bansak, Hainmueller, Hopkins, and Yamamoto (2023)](https://doi.org/10.1017/pan.2022.16) on the interpretation of AMCEs as effects on expected vote share. The argument presented here concerns identifying tradeoffs, not the validity or usefulness of the AMCE as an electoral estimand.

### Averaging over contests 

Second, the AMCE averages across contests that impose different costs of rejecting an undesirable feature, even within a respondent subgroup. For simplicity, consider only Democratic respondents, and suppose all of them have the same preferences and attach the same electoral penalty to gerrymandering and the same value to supporting their own party. They choose between a Democratic candidate who runs against either another Democrat or a Republican. Let $G=1$ indicate the gerrymandering candidate and $G=0$ the non-gerrymandering candidate. To simplify, the opponent always rejects gerrymandering. 

| Opponent | Support for Democrat, $G=0$ | Support for Democrat, $G=1$ | AMCE of $G$ |
|---|---:|---:|---:|
| Another Democrat | 50.0% | 40.1% | −9.9 percentage points |
| A Republican | 76.9% | 69.0% | −7.9 percentage points |

Gerrymandering reduces support in both comparisons. However, against another Democrat, a Democratic respondent can reject gerrymandering while retaining partisan alignment. Against a Republican, punishing gerrymandering requires voting for the opposing party, and only respondents that were already close to indifferent between the two candidates switch. As the cost of gerrymandering and partisan alignment are the same in both rows by construction, the smaller electoral penalty does not reflect a stronger tolerance of gerrymandering nor how much respondents value partisan alignment relative to gerrymandering. 

### Averaging over contests: the weights

Third, and related, the AMCE depends on the distribution of profile comparisons over which effects are averaged and so depends on how frequently the experiment presents each comparison, including those that require a tradeoff. Continuing the example, now allow either opponent party to propose or reject gerrymandering, as in a standard conjoint experiment.[^focal]

[^focal]: The example below holds the candidate's party fixed at Democrat. In a fully randomized paired design the AMCE also averages over the focal candidate's party. In this simplified example, the average over the eight resulting cells equals the average over the four shown here as these are simply a permutation of the four shown here. 

| Opponent | Support for Democrat, $G=0$ | Support for Democrat, $G=1$ | AMCE of $G$ |
|---|---:|---:|---:|
| Democrat, rejects gerrymandering | 50.00% | 40.13% | −9.87 percentage points |
| Republican, rejects gerrymandering | 76.85% | 69.00% | −7.86 percentage points |
| Democrat, proposes gerrymandering | 59.87% | 50.00% | −9.87 percentage points |
| Republican, proposes gerrymandering | 83.20% | 76.85% | −6.35 percentage points |
| Pooled AMCE |        |        | −8.5 percentage points |

The second row is the only comparison in which a Democratic respondent faces the tradeoff between electoral integrity and partisan loyalty, and only when the Democrat proposes gerrymandering. In the other three rows the Democrat either competes with another copartisan, so that proposing gerrymandering is a pure cost with no benefit, or faces a Republican who also proposes gerrymandering, and so has no disadvantage. The second row receives one quarter of the weight in the AMCE decomposition and the three rows that pose no tradeoff receive three quarters. As the rows that pose no tradeoff carry the larger effects, the AMCE overstates the penalty.

This illustrates the profile-distribution issue discussed in [de la Cuesta, Egami, and Imai (2022)](https://doi.org/10.1017/pan.2020.40). Even with unchanged preferences and unchanged support in every individual contest, the distribution used for averaging becomes part of the substantive meaning of the AMCE[^distribution]. As a consequence, the AMCE can be larger or smaller because more weight is given to contests that pose no tradeoff.[^interaction][^logit3]

[^distribution]: Distribution dependence is a property of the estimand. It does not mean that its estimator is biased. Changing the averaging distribution changes the AMCE when it changes the weighting of unequal conditional effects. It has no effect when those conditional effects are constant. Estimation under a target distribution requires adequate experimental support and suitable weighting or modeling assumptions. See [de la Cuesta, Egami, and Imai (2022)](https://doi.org/10.1017/pan.2020.40).

[^interaction]: If the cost of gerrymandering itself depended on the opponent, for instance because Democratic respondents punish a copartisan's gerrymandering more harshly than a Republican's, the rows would differ in what is being traded and not only in the starting level of support. If this is the case, the problem gets worse and even a row-by-row comparison would not isolate a single cost of gerrymandering. See [Egami and Imai (2019)](https://doi.org/10.1080/01621459.2018.1476246) on attribute interactions in conjoints. 

[^logit3]: The coefficient of a logit model does not depend on the weights, for a reason the table makes visible once support is expressed as odds. The odds of choosing the Democrat fall by the same third in all four rows, from 50:50 to 40:60, from 77:23 to 69:31, and so on. On the log-odds scale on which the coefficient is measured the four rows show the same drop of 0.40, and an average of four equal numbers is the same whatever the weights. 

### Subgroup analysis

Some conjoint studies turn to subgroup analysis assuming that a difference in AMCEs across groups can be interpreted as evidence for tradeoffs. [Neundorf et al. (2026)](https://doi.org/10.1017/S0007123425101129), for example, report larger AMCEs for democratic attributes among respondents with stronger prior support for democracy and conclude that "citizens with higher support for democracy prioritize living in a democracy relative to [economic] security." Subgroup analysis can reveal differences in average support that are hidden in the pooled AMCE. However, it is subject to the same issues presented above and, additionally, raises a related issue[^mm]. To illustrate, let's return to the Democratic respondents of the previous example, but now add a second group of Democrats whose cost of gerrymandering and value of a copartisan are exactly twice as large[^twice]. The second group ranks the two attributes exactly as the first does. 

[^mm]: [Leeper, Hobolt, and Tilley (2020)](https://doi.org/10.1017/pan.2019.30) show another issue with AMCEs. Differences in AMCEs across subgroups also depend on which attribute level serves as the reference category. The authors recommend using marginal means instead. Marginal means remove the reference category issue but they average over the same distribution of contests as the AMCE and are therefore subject to the second and third issues above.

[^twice]: This implies that these individuals are more consistent and fewer of their choices are close calls.  

| Opponent | Support for Democrat, $G=0$ | Support for Democrat, $G=1$ | Effect of $G$ |
|---|---:|---:|---:|
| Democrat, rejects gerrymandering | 50.00% | 31.00% | −19.00 percentage points |
| Republican, rejects gerrymandering | 91.68% | 83.20% | −8.48 percentage points |
| Democrat, proposes gerrymandering | 69.00% | 50.00% | −19.00 percentage points |
| Republican, proposes gerrymandering | 96.08% | 91.68% | −4.40 percentage points |
| **Pooled AMCE of gerrymandering** | | | **−12.72 percentage points** |

In this example, the AMCE of gerrymandering is −12.72 points in the second group against −8.5 in the first. Following the logic of [Neundorf et al. (2026)](https://doi.org/10.1017/S0007123425101129), we would conclude that the second group prioritizes fair elections over partisanship. Yet the exact opposite holds in the one contest that actually presents the tradeoff. When a gerrymandering Democrat faces a Republican who rejects gerrymandering, the second group chooses the Democrat with probability 83.20%, compared with 69.00% in the first. Only 16.8% of the second group defect to punish gerrymandering, compared to 31.0% in the first group. A larger average electoral penalty therefore coexists with higher willingness to trade off fair elections for partisan loyalty.[^scale] This reveals that the size of a subgroup AMCE depends on how consistently its members act on their ranking of the attributes, not only on the ranking. Two groups with the same ranking can show very different AMCEs, and the group with the larger AMCE can be the one more willing to make the tradeoff. Restricting the analysis to a subgroup therefore leaves all three of the preceding distinctions in place, and adds the one just shown. 

[^scale]: This paradox stems from the scale confound well known in discrete choice modeling ([Swait and Louviere 1993](https://doi.org/10.1177/002224379303000303)). The coefficients (and marginal effects) of a logit/probit model are identified only relative to the unobserved variance, so a group whose choices are less noisy shows larger effects on every attribute. 

### Ratio of AMCEs

Some scholars ([Adserà, Arenas, and Boix 2023](https://doi.org/10.1073/pnas.2306168120); [Ferrer et al. 2025](https://doi.org/10.1111/1475-6765.70021)) divide the AMCE of one attribute by the AMCE of another to express the first in units of the second. The ratio is read as a tradeoff, i.e., the increase in one attribute, e.g., income, required on average to persuade respondents to give up another, e.g., gerrymandering. It addresses the scale issue exposed above, i.e., a penalty is large or small only relative to what else moves support. 

It does not, however, resolve the other issues presented above and adds some more. First, it remains a ratio of population averages which is subject to the aggregation problem and dependent on the profile distribution and its weights. Second, a ratio of average effects is not the average of the respondents' ratios. It weights each respondent in proportion to their sensitivity to the compensating attribute, so respondents who are easily compensated dominate the estimate, respondents who are hard to compensate barely enter it, and respondents whom no amount of the compensating attribute would persuade do not enter it at all. Third, each AMCE is bounded by the design. In a paired design where both candidates' levels are randomized, no binary attribute can move support by more than 50 points, because in half of the contests both candidates share the same level, and the bound widens with the number of levels an attribute has ([Ganter 2023](https://doi.org/10.1017/pan.2021.41)), so that a ratio of AMCEs across attributes with different numbers of levels is partly a ratio of design features.[^ratioscale] To illustrate, when respondents always put party first and reject gerrymandering only when party is tied, the AMCEs are −25 and +50 points whatever the exchange rate, and the ratio reports the design and the ranking rather than the rate.

[^ratioscale]: The latent-utility model presented below avoids the third problem, and the second only when it models heterogeneity ([Daly, Hess, and Train 2012](https://doi.org/10.1007/s11116-011-9331-3)). Its coefficient ratio is unbounded, and it is invariant to how predictable a group's choices are because the unobserved variance cancels between numerator and denominator. 

In sum, an AMCE combines four things: 1) how much each respondent values an attribute and whether respondents agree on its direction, 2) how much they value everything else in the contest, 3) how often the experiment shows each contest, and 4) how consistently respondents act on their preferences. The AMCE is a meaningful measure of electoral reward or punishment. It is not in general a measure of how much improvement in another attribute compensates respondents for one they dislike. Separate AMCEs, their differences across attributes or across subgroups, and their ratios are not a measure of tradeoff. Subgroup analysis, marginal means, and alternative averaging distributions can make the electoral effect more substantively relevant, but also do not identify a tradeoff.

## How to (better) identify tradeoff using conjoint data 

The quantity of interest when measuring tradeoff is whether, and to what extent, a respondent who dislikes one attribute accepts it anyway to obtain something that they value. There are two ways to approach it with conjoint data.

The first asks how often respondents choose the profile that combines the benefit with the drawback over the profile that offers neither, in a contest in which the remaining attributes are held equal. In the running example above it is the 69 percent of Democrats who choose the gerrymandering Democrat over a clean Republican. The quantity needs no model but assumes that all the respondents penalize one attribute and reward the compensating attribute (monotonicity).[^regression] It uses only the tasks that pose the tradeoff, which in a standard conjoint design are few.[^power] In addition, it does not tell us whether respondents, on average, value one attribute and dislike the other. A conjoint experiment should therefore not consist of tradeoff contests alone. Contests that pose no tradeoff supply the information needed to establish that, on average, the drawback lowers support and the benefit raises it. In the example above, support for the Democrat falls when she alone proposes gerrymandering against another Democrat, and rises when a clean Democrat faces a clean Republican.

[^power]: For power analysis and experimental design (as implemented in `cjsimPWR`), direct tradeoff identification requires estimating support exclusively from tasks that pose the tradeoff. In a paired design with two binary attributes under independent uniform randomization, tradeoff contests constitute exactly one eighth (12.5%) of all tasks ($2/16$). This share decreases exponentially when experiment attributes are matched to respondent-varying characteristics (e.g., to measure copartisanship). A tradeoff analysis therefore needs a larger sample, or an (optimal) design that oversamples the tradeoff tasks. [Ganter (2023)](https://doi.org/10.1017/pan.2021.41) makes a related argument for restricting attention to the direct comparisons that identify a preference between two levels of one attribute.

[^regression]: This can also be estimated using a regression that includes the opponent's attributes and their interactions with the focal candidate's, or be run on the tradeoff tasks only, where the fitted value is the cell mean.

The second asks how much of the benefit would make each respondent indifferent between accepting the drawback and avoiding it. This is a compensating amount, defined as the ratio of the two attributes' coefficients, substantively similar to the AMCE ratios detailed above. For a continuous benefit, the ratio is an amount in the benefit's units. For two binary attributes the ratio describes the cost of the drawback as a fraction of the value of the benefit. The respondent accepts the compromise when that fraction is below one. The ratio can be estimated using a simple latent-utility model using the coefficient on the drawback attribute and the coefficient on the benefit attribute. Because the choice depends only on which profile has more utility, attributes the two profiles share cancel out, and the probability of choosing a profile is a logit of the differences between the two profiles in each attribute. This approach uses all tasks rather than only the tradeoff tasks, but it does so at the cost of three assumptions. First, the cost and value are the same in every contest, so that no interaction between an attribute and the opponent's attributes is needed. Second, the two add up on a single scale and follow a logistic curve. This lets a cost measured where choices are close predict its effect where they are not. Third, preferences must be homogeneous so that every respondent dislikes the drawback and values the benefit.

The two approaches describe two sides of the same coin. A respondent chooses the compromise exactly when the benefit on offer exceeds her compensating amount, so the choice frequency at a given level of the benefit is the share of respondents whose compensating amount lies below that level. The first approach reports one point of the distribution of compensating amounts, at the level the design happens to show, with few assumptions. The second reports the distribution itself, or a summary of it, and requires additional assumptions. [Graham and Svolik (2020)](https://doi.org/10.1017/S0003055420000052) illustrate both approaches in one study. They measure the share of respondents who defect from a candidate who adopts an undemocratic position in contests where defecting requires voting against one's party or policy interests, and they estimate a logit model whose coefficient ratios express the value of democracy in units of policy distance and of co-partisanship.

### Outstanding issues with a logit MRS [TO DEVELOP]

- A ratio of two population summaries is not a summary of the individual ratios and behaves like the ratio of AMCEs. It is close to the average of the respondents' exchange rates weighted by their sensitivity to the compensating attribute, not to their unweighted average. We need a mixed logit. 
- Even with a mixed logit, when some respondents are nearly insensitive to the compensating attribute their ratios are arbitrarily large. Their distribution of ratios has no finite mean ([Daly, Hess, and Train 2012](https://doi.org/10.1007/s11116-011-9331-3)). Bounding the distribution away from zero restores the mean but introduces an important assumption: nobody is close to indifferent. To resolve, just report share and the median so that we know how many respondents lie below 1. Mean is biased. 





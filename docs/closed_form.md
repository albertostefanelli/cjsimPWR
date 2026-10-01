Power to detect an average marginal component effect (AMCE) can be
computed in closed form, instantly and without simulation error. In
most designs, the closed form gives the same power as simulation.
Simulation is nonetheless the package's default, because the two
differ in some situations that are common in practice. 

# Closed form 

## The task-level estimator

Consider $N$ respondents who each complete $T$ tasks, choosing one of
two profiles in each. Focus on one attribute with $L$ levels and on
one single comparison, between a level of interest and a reference
level. Respondent $i$'s AMCE, $\tau_i$, is how much more likely that
respondent is to choose a profile when it shows the level of interest
than when it shows the reference level, averaging over everything
else the design randomizes. The population AMCE, $\tau$, is the
average of these individual effects, and $\sigma$ is their standard
deviation, i.e., how much the effect varies from one respondent to
another. Assume, as in a standard conjoint design, that each
attribute's level is drawn independently of the other's, with all $L$
levels equally likely.

For each of a task's two profiles, $j = 1, 2$, let $h_j = 1$ if the
profile shows the level of interest, $h_j = -1$ if it shows the
reference level and $h_j = 0$ if it shows any other level. Let $Y_1 =
1$ if the first profile is chosen and $Y_1 = 0$ if the second is. The
task's score is

$$ S = \frac{L}{2}\,(h_1 - h_2)\left(Y_1 - \frac12\right). $$


$S$ indicates whether the choice favoured the level of interest or the
reference level. The term $Y_1 - \frac12$ equals $+\frac12$ when the
first profile is chosen and $-\frac12$ when the second is, so it
records which profile won. The term $h_1 - h_2$ records which choice
favour the level of interest. If it is positive, the choice favours
the level of interest; if it is negative, it favours the reference
level. If $h_1 - h_2 = 0$, the score is zero whichever profile is
chosen. This happens when (a) both profiles show the same level or
(b) neither profile shows the level of interest or the reference
level. The latter is possible only when the attribute has more than
two levels.

For a binary attribute, $L/2 = 1$ and the score is a simple win–loss
record. Take a candidate's gender, with male as the level of interest
and female as the reference level: $S = 1$ when a man is chosen over
a woman, $S = -1$ when a woman is chosen over a man, and $S = 0$ when
both candidates are men or both are women.[^example_binary]

[^example_binary]: To see that the average score equals the AMCE, take
1,000 tasks. Under randomization, about 500 show one man and one
woman, 250 show two men and 250 show two women. If the man is chosen
in 55 percent of the mixed tasks, 275 tasks score +1, 225 score −1
and the other 500 score zero, so the average score is (275 −
225)/1,000 = 0.05. The AMCE is also 0.05: of the 1,000 profiles
showing a man, 525 are chosen (275 against a woman, plus one man in
each of the 250 tasks with two men), against 475 of the 1,000 showing
a woman.

With more than two levels, a task also counts when only one of the two
levels appears, against a third level. If the level of interest wins,
or the reference level loses, the task scores $L/4$, because either
outcome widens the gap between how often the two levels are chosen;
the reverse outcomes score $-L/4$. A task showing both levels counts
double, $\pm L/2$, because one choice is then a win for one level and
a loss for the other.[^example_nonbinary]

[^example_nonbinary]: For example, take a candidate's age, with four
levels (40, 50, 60 and 70), 50 as the level of interest and 40 as the
reference level, so that $L/4 = 1$ and $L/2 = 2$. A task scores +1
when a 50-year-old beats a 60- or 70-year-old, or when a 40-year-old
loses to one, and −1 when the reverse happens. A task with a
50-year-old and a 40-year-old scores +2 if the 50-year-old is chosen
and −2 if the 40-year-old is.

The factor $L/2$ puts the score on the scale of the AMCE. Only
profiles that show the level of interest or the reference level add
to the score, and because all levels are equally likely, they make
up, on average, $2/L$ of all profiles. The remaining profiles add
zeros, which would shrink the average score to $2/L$ of the AMCE;
multiplying by $L/2$ corrects this.[^dilution] The average of all
$NT$ scores estimates the population AMCE:

[^dilution]: With two levels, as for gender, every profile shows one
of the two levels being compared, so nothing is diluted and $L/2 =
1$. With four, as for age, only half of the profiles show 50 or 40;
the other half show 60 or 70 and add zeros, so without the factor the
average score would be half the AMCE, and multiplying by $L/2 = 2$
restores it.

$$
\widehat{\tau} = \frac{1}{NT}\sum_{i=1}^{N}\sum_{t=1}^{T} S_{it}, 
$$

where $S_{it}$ is the score of respondent $i$'s $t$-th task. Because
each task's expected score is its respondent's AMCE, and respondents
are sampled at random, $\widehat{\tau}$ is unbiased: on average
across samples, it equals $\tau$. Because it is a simple average, its
variance, and hence power, also has a closed form.

## Sampling variance

Under the assumptions of a standard conjoint design,[^assumptions] the
variance of $\widehat{\tau}$ depends on how much a single task's
score varies and on how strongly two scores from the same respondent
are related.

[^assumptions]: Respondents are sampled independently of one another;
all profiles, including the two in each task, are drawn independently
of one another, with all levels of each attribute equally likely; and
each respondent's preferences stay the same from task to task and,
given those preferences, the choice in one task does not affect the
choice in another, so that there is no learning, fatigue or
carryover. The second is a form of the randomization assumption of
Hainmueller, Hopkins and Yamamoto(2014), and the third is their
stability and no-carryover assumption.

Because $(Y_1 - \frac12)^2 = \frac14$ whichever profile is chosen, the
squared score, $S^2 = \frac{L^2}{16}(h_1 - h_2)^2$, depends only on
the levels the task shows, and its average over the randomization is
$L/4$ for any number of levels.[^squared_score] Since the expected
score is $\tau$, the variance of a task's score is $L/4 - \tau^2$.

[^squared_score]: Each profile shows one of the two compared levels,
so that $h_j^2 = 1$, with probability $2/L$. The product $h_1 h_2$
averages zero, because the two profiles are drawn independently and
each is as likely to show the level of interest as the reference
level. The average of $(h_1 - h_2)^2 = h_1^2 + h_2^2 - 2h_1h_2$ is
therefore $4/L$, and that of $S^2$ is $\frac{L^2}{16} \cdot \frac{4}
{L} = \frac{L}{4}$. For gender, take 1,000 tasks: about 500 show one
man and one woman and score ±1, and the other 500 score zero, so the
average squared score is 500/1,000 = 1/2 = $L/4$, whichever
candidates are chosen.

Scores from different respondents are independent. Two scores from the
same respondent share that respondent's AMCE, $\tau_i$, as their
expected value and, given the respondent's preferences, are otherwise
independent, so their covariance is the variance of the individual
AMCEs, $\sigma^2$. For gender ($L = 2$, so $L/4 = 0.5$), suppose that
half the respondents prefer men, with an AMCE of 0.20, and half
prefer women, with an AMCE of −0.10, so that $\tau = 0.05$ and
$\sigma = 0.15$. A respondent who prefers men has an expected score
of 0.20 in every task, and one who prefers women −0.10, so two scores
from the same respondent tend to be high together or low together.
[^covariance]

[^covariance]: Given the respondent, two tasks are independent, so the
expected product of their scores is $\tau_i^2$, whose average across
respondents is $\tau^2 + \sigma^2$. Subtracting the product of the
two expected scores, $\tau^2$, leaves a covariance of $\sigma^2$. In
the gender example, the expected product is $0.20^2 = 0.04$ for a
respondent who prefers men and $(-0.10)^2 = 0.01$ for one who prefers
women, or 0.025 on average, and subtracting $\tau^2 = 0.0025$ gives
$0.0225 = 0.15^2$.

The estimate averages $NT$ scores: $T$ from each of $N$ independent
respondents, with covariance $\sigma^2$ between any two from the same
respondent. Its variance, $V$, is therefore[^variance_derivation]

[^variance_derivation]: The sum of one respondent's $T$ scores has
variance $T(L/4 - \tau^2) + T(T - 1)\sigma^2$. These are the variances of the
$T$ scores plus twice the covariances of the $T(T - 1)/2$ pairs of
scores. Respondents are independent, so the sum of all $NT$ scores
has $N$ times this variance, and dividing by $(NT)^2$ gives the first
line. The second line writes $(T - 1)\sigma^2$ as
$T\sigma^2 - \sigma^2$.

$$
\begin{aligned} V &= \frac{L/4 - \tau^2 + (T - 1)\,\sigma^2}{NT} \\
  &= \frac{L/4 - \tau^2 - \sigma^2}{NT} + \frac{\sigma^2}{N}.
\end{aligned} 
$$

The second line separates two sources of error. The first is
measurement noise: a task's score varies around its respondent's
AMCE, $\tau_i$, with a variance that averages
$L/4 - \tau^2 - \sigma^2$ across respondents,[^noise] and averaging
over all $NT$ tasks divides it by $NT$, giving the first term. The
second is that respondents' AMCEs differ from $\tau$, with variance
$\sigma^2$. Each respondent's difference recurs in all of that
respondent's tasks, so only more respondents reduce it, giving the
second term, $\sigma^2/N$. In other words, more tasks reduce
measurement noise and more respondents reduce sampling variation.
[^floor]

[^noise]: Given the respondent, the expected squared score is still
$L/4$ and the expected score is $\tau_i$, so a task's score varies
around $\tau_i$ with variance $L/4 - \tau_i^2$. Because the average
of $\tau_i^2$ across respondents is $\tau^2 + \sigma^2$, this
variance averages $L/4 - \tau^2 - \sigma^2$. 

[^floor]: However many tasks each respondent completes, $V$ cannot
fall below $\sigma^2/N$, the variance of the average AMCE of $N$
randomly sampled respondents.

Equivalently, $V$ is the variance that $NT$ tasks by $NT$ different
respondents would give, multiplied by a factor that reflects
clustering:

$$ V = \frac{L/4 - \tau^2}{NT}\,\bigl[1 + (T - 1)\,\rho\bigr],
\qquad
\rho = \frac{\sigma^2}{L/4 - \tau^2}. $$

Here $\rho$ is the correlation between two scores from the same
respondent, their covariance divided by their variance, and $1 +
(T - 1)\rho$ is the design effect of cluster sampling (Kish 1965),
with respondents as clusters. Because $\rho$ is multiplied by $T -
1$, even a small correlation raises the variance considerably when
respondents complete many tasks.[^design_effect]

[^design_effect]: In the gender example, $\rho = 0.0225/
(0.5 - 0.0025) \approx 0.045$. With 10 tasks per respondent, the
design effect is $1 + 9 \times 0.045 \approx 1.41$: the variance is
41 percent larger than if each task came from a different respondent.
With 50 tasks per respondent, it is more than three times as large.

## Power and sample size

Power then follows from the standard formula: if $\widehat{\tau}$ is
normal around $\tau$ with variance $V$, the power of a two-sided test
that the AMCE is zero, at significance level $\alpha$, is

$$
\Phi\!\left(\frac{|\tau|}{\sqrt{V}} - z_{1-\alpha/2}\right) +
\Phi\!\left(-\frac{|\tau|}{\sqrt{V}} - z_{1-\alpha/2}\right), $$

where $\sqrt{V}$ is the standard error of the estimate, $|\tau|/\sqrt
{V}$ the size of the effect in standard errors, $\Phi$ the standard
normal cumulative distribution function and $z_p = \Phi^{-1}(p)$. The
first term is the probability that the estimate is significant and
has the same sign as $\tau$; the second is the probability that it is
significant with the opposite sign, which is negligible unless power
is low. When $\tau = 0$, each term equals $\alpha/2$, and the formula
returns $\alpha$, the probability of rejecting a true null. 

Since $V$ is the variance of one
respondent's average score divided by $N$, the number of respondents
needed is

$$ N \approx
\left(\frac{L/4 - \tau^2 - \sigma^2}{T} + \sigma^2\right)
\left(\frac{z_{1-\alpha/2} + z_\pi}{|\tau|}\right)^2. $$

The first factor is the variance of one respondent's average score,
which combines the two sources of error above. The second sets the
precision needed to detect the effect and grows quickly as the effect
shrinks: halving $\tau$ roughly quadruples $N$. As $T$ grows, the
first factor falls towards $\sigma^2$, so however many tasks each
respondent completes, at least $\sigma^2 (z_
{1-\alpha/2} + z_\pi)^2/\tau^2$ respondents are needed; with fewer,
no number of tasks reaches the target power. 

## Comparison with simulation

Under the assumptions of a standard conjoint design, $V$ is the exact
variance of $\widehat{\tau}$, and in simple designs the closed form
gives the same power as simulation. The two differ in three
situations: when other attributes have sizeable effects and when
respondents are few, both common in practice, and when the requested
effects describe no possible population.

The first situation arises because respondents weigh all attributes at
once when decising which profile to choose. Since the other attributes are randomized independently of the
focal one, conditioning on them leaves the estimand unchanged but
absorbs part of the variation in choices, reducing the sampling
variance of the estimated AMCE, just as regression adjustment for
pre-treatment covariates does in a randomized experiment (Lin 2013).
The closed form estimator makes no such adjustment. Its variance,
$V$, does not depend on the other attributes' effects, so the it
understates power and yields conservative sample sizes[^sigma]. 

[^sigma]: This gain in precision shrinks as respondents differ more.
The other attributes reduce the measurement noise within each
respondent, the first term of $V$, but not the differences between
respondents, the second term, whose share of $V$ grows with
$\sigma$.

The second situation concerns the test. The closed form treats the
standard error as known and normal critical values as exact, so it
assumes that the test rejects a true null exactly $\alpha$ of the
time and that 95 percent confidence intervals contain the true AMCE
95 percent of the time. With few respondents or small subgroups that estimate is
noisy and tends to be too small, and normal critical values are too
permissive --- the small-sample problems motivating the corrections in
[Pustejovsky and Tipton (2018)]
(https://doi.org/10.1080/07350015.2016.1247004). More tasks do not
help, because they add choices, not respondents, and the problem
arises even when all respondents share the same AMCE. 

The third situation concerns the effects themselves. An AMCE is a
difference between two choice probabilities, so it cannot be
arbitrarily large: in a paired design, no respondent's AMCE can
exceed $1 - 1/L$ in size, however strong the respondent's preference.
Take the most extreme gender respondent, who always picks the man
when the candidates differ. That respondent's AMCE is only 0.5,
because in half the tasks both candidates are men or both are women,
and in those tasks the choice cannot favour either gender.[^extreme]

[^extreme]: In general, the level of interest can be chosen in at most
$1 - 1/(2L)$ of the profiles that show it, and the reference level in
no fewer than $1/(2L)$, because whenever both profiles show the same
level, one of them is chosen. For gender, take 1,000 tasks. Of the
1,000 profiles showing a man, the extreme respondent chooses 750, all
500 against a woman plus one in each of the 250 tasks with two men;
of the 1,000 showing a woman, only 250, one in each task with two
women. The AMCE is $0.75 - 0.25 = 0.5 = 1 - 1/L$.

The same applies to several effects requested together: each can be
possible on its own but impossible in combination.[^joint] The closed
form looks at one effect at a time. The simulation, instead, has to build actual
respondents whose choices produce all the requested effects at once,
so it notices when no such population exists and stops.

[^joint]: Across the levels of an attribute, profiles are chosen half
the time on average, so raising some levels' choice rates lowers the
others'. For a three-level attribute, AMCEs of 0.6 for each
non-reference level are each possible on their own, since 0.6 is
below $1 - 1/3 \approx 0.67$. Together, they would require the
reference level to be chosen in only 10 percent of the profiles that
show it, below the minimum of one in six set by the tasks in which
both profiles show it.


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_cj.R
\name{sim_cj}
\alias{sim_cj}
\title{Generate and evaluate a conjoint experiment based on the design specification.}
\usage{
sim_cj(
  n_attributes = NULL,
  n_levels = NULL,
  group_name = NULL,
  units = NULL,
  n_tasks = NULL,
  true_coef = NULL,
  sigma.u_k = NULL,
  n_sim = NULL,
  prog = FALSE,
  p
)
}
\arguments{
\item{n_attributes}{int; number of independent variables (i.e. treatments) in the conjoint experiment.}

\item{n_levels}{int; vector of integers of length n_attributes; number of levels of variables should be > 1.}

\item{group_name}{character; optional vector of names for the observed populations/subgroups of respondents.}

\item{units}{int; single or a list indicating the number of respondents.}

\item{n_tasks}{int;  single or a list indicating number of tasks performed by each respondent.}

\item{true_coef}{vector or list of vectors; hypothesized (true) coefficients for all parameters to be estimated. The coefficients must be in order, e.g., var_1_lvl_1, var_1_lvl2, var_2_lvl1, var_2_lvl2.}

\item{sigma.u_k}{int; standard deviation of the treatment effect for each variable/level for each respondent.}

\item{n_sim}{internal parameter, integer; number of simulations specified by power_sim().}

\item{prog}{internal parameter, logical; whether the progress indicator should be activated to feed power_sim(). Default set to FALSE.}

\item{p}{internal parameter, numeric; passes the progress indicator to power_sim().}
}
\value{
The function return a dataframe containing the following parameters:
\itemize{
\item{num_respondents:} {int; total number or respondents.}
\item{num_attrbs:} {int; total number of attributes included in the conjoint experiment.}
\item{num_tasks:} {int; total number of task taken by each respondent.}
\item{id_grp:} {character; name of the sub-population of interest.}
\item{attrb:} {int; name of the attribute included in the conjoint design. At the moment these cannot be changed and are in the format var_1 ... var_x.}
\item{level:} {int; levels of each attribute.}
\item{true_coef:} {numeric; coefficient from true_coefs.}
\item{estimate:} {numeric; estimated coefficient from the linear probability model.}
\item{std.error[.robust]:} {numeric; estimated (robust) standard error.}
\item{ci[_robust]_lower:} {numeric; lower bound of the confidence interval calculated (level= 0.95).}
\item{ci[_robust]_upper:} {numeric;upper bound of the confidence interval calculated (level= 0.95).}
\item{in_ci[_robust]:} {logical, if the true_coef is contained in the confidence level.}
\item{sig[_robust]:} {logical, if the estimate coefficent (estimate) is significant (alpha = 0.05).}
\item{typeS[_robust]:} {logical, did the sign of sig. coef match to true? (see, Gelman and Carlin, 2014).}
\item{typeM[_robust]:} {numeric; ratio of estimated/true coefficent, if significant (see Gelman and Carlin, 2014).}
}
}
\description{
The function constructs a full factorial experimental design based on the design specification, generates samples of conjoint profiles drawing from the full factorial design, and evaluate the simulated conjoint process of profile selection against the hypothesized true coefficients.
}
\examples{

# Evaluation of a conjoint design with no observed sub-groups of respondents
df_power <- sim_cj(
          n_attributes = 3,
          n_levels = c(2, 3, 5),
          n_tasks = 4,
          units = c(500),
          true_coef = list(0.2, c(-0.1, 0.1), c(-0.1, -0.1, -0.1, 0.1)),
          sigma.u_k = 0.05,
          prog = FALSE)

# Evaluation of a conjoint design with 3 observed sub-groups of respondents.
df_cj <- sim_cj(
          n_attributes = 3,
          n_levels = c(2, 3, 5),
          n_tasks = 3,
          group_name = c("Democrat", "Independent", "Republican"),
          units = c(500, 200, 500),
            true_coef = list("Democrat" = list(0.2, c(-0.1, 0.1), c(-0.1, -0.1, -0.1, 0.1)),
                             "Independent" = list(0.1, c(-0.2, -0.05),  c(-0.1, 0.1, 0.1, 0.3)),
                             "Republican" = list(0.1, c(-0.1, 0.05),  c(-0.1, 0.2, -0.1, 0.1))
                             ),
         sigma.u_k = 0.05,
         prog = FALSE)

}

#' @details
#' Use [power_sim()] to simulate repeated paired, forced-choice conjoint experiments and report
#' power, Type I error, coverage, and Type S and Type M errors with Monte Carlo standard errors.
#' The default choice model is calibrated to requested average marginal component effects (AMCEs),
#' optionally with respondent heterogeneity and respondent subgroups. Estimates use
#' respondent-clustered standard errors.
#'
#' For individual steps, use [conjoint_design()] to describe the attributes,
#' [simulate_experiment()] to draw an experiment, [estimate_amce()] to analyse it,
#' and [summarise_runs()] to summarise estimates from repeated experiments.
#' Run `news(package = "cjsimPWR")` for release notes and `citation("cjsimPWR")` for citations.
#'
#' @seealso [power_sim()], [conjoint_design()], [simulate_experiment()], [estimate_amce()],
#'   [summarise_runs()]
#' @md
"_PACKAGE"

#' @details
#' [power_sim()] runs the whole analysis described above. For individual steps, use
#' [conjoint_design()] to describe the attributes, [simulate_experiment()] to draw one experiment,
#' [estimate_amce()] to analyse it, and [summarise_runs()] to summarise estimates from repeated
#' experiments.
#'
#' Full explanations live in the package guides, online:
#' [simulation model](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/simulation_model.md),
#' [clustering and inference](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/clustering.md),
#' [power, Type I, Type S and Type M errors](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/error_rates.md),
#' [calibration and reference precision](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/calibration.md),
#' [comparison to closed-form power formulas](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/closed_form.md) and
#' [comparison to DeclareDesign](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/declaredesign.md).
#' Run `news(package = "cjsimPWR")` for release notes and `citation("cjsimPWR")` for citations.
#'
#' @seealso [power_sim()], [conjoint_design()], [simulate_experiment()], [estimate_amce()],
#'   [summarise_runs()]
#' @md
"_PACKAGE"

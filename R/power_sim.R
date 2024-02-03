#' Generate and evaluate a conjoint experiment based on the design specification.
#' @description The function constructs a full factorial experimental design based on the design specification, generates samples of conjoint profiles drawing from the full factorial design, and evaluate the simulated conjoint process of profile selection against the hypothesized true coefficients.
#'
#' @param n_attributes int; number of independent variables (i.e. treatments) in the conjoint experiment.
#' @param n_levels int; vector of integers of length n_attributes; number of levels of variables should be > 1.
#' @param group_name character; optional vector of names for the observed populations/subgroups of respondents.
#' @param units int; single or a list indicating the number of respondents.
#' @param n_tasks int;  single or a list indicating number of tasks performed by each respondent.
#' @param true_coef vector or list of vectors; hypothesized (true) coefficients for all parameters to be estimated. The coefficients must be in order, e.g., var_1_lvl_1, var_1_lvl2, var_2_lvl1, var_2_lvl2.
#' @param sigma.u_k int; standard deviation of the treatment effect for each variable/level for each respondent.
#' @param sim_runs int; number of simulations to perform. Larger numbers correspond to more precise estimates.
#' @param seed  int; the initial seed used in the simulation. The RNG reproducibility is achieved by pre-generating the random seeds for all iterations of the simulation such that the seeds always returns the exact same sequence of random numbers given the specified seed. See future_lapply for more information.
#' @param n_sim internal parameter, numeric; number of the simulation from sim_cj to power_sim().
#' @param p internal parameter, numeric; passes the progress indicator from sim_cj() to power_sim().
#'
#' @return The function returns a dataframe containing the results of the evaluation procedure and the design specification
#' \itemize{
#' \item{num_respondents:} {int; total number or respondents.}
#' \item{num_attrbs:} {int; total number of attributes included in the conjoint experiment.}
#' \item{num_tasks:} {int; total number of task taken by each respondent.}
#' \item{id_grp:} {character; name of the sub-population of interest.}
#' \item{attrb:} {int; name of the attribute included in the conjoint design. At the moment these cannot be changed and are in the format var_1 ... var_x.}
#' \item{level:} {int; levels of each attribute.}
#' \item{true_coef:} {numeric; coefficient from true_coefs.}
#' \item{estimate:} {numeric; estimated coefficient from the linear probability model.}
#' \item{std.error[.robust]:} {numeric; estimated (robust) standard error.}
#' \item{ci[_robust]_lower:} {numeric; lower bound of the confidence interval calculated (level= 0.95).}
#' \item{ci[_robust]_upper:} {numeric;upper bound of the confidence interval calculated (level= 0.95).}
#' \item{in_ci[_robust]:} {logical, if the true_coef is contained in the confidence level.}
#' \item{sig[_robust]:} {logical, if the estimate coefficent (estimate) is significant (alpha = 0.05).}
#' \item{typeS[_robust]:} {logical, did the sign of sig. coef match to true? (see, Gelman and Carlin, 2014).}
#' \item{typeM[_robust]:} {numeric; ratio of estimated/true coefficent, if significant (see Gelman and Carlin, 2014).}
#' }
#' @export
#'
#' @examples
#'
#' # Simulated power, Type M, and Type S error
#' # for a conjoint design with no observed sub-groups of respondents.
#' df_wo_subgroups <- power_sim(
#'           n_attributes = 3,
#'           n_levels = c(2, 3, 5),
#'           n_tasks = 4,
#'           units = c(500),
#'           true_coef = list(0.2, c(-0.1, 0.1), c(-0.1, -0.1, -0.01, 0.1)),
#'           sigma.u_k = 0.05,
#'           seed = 92,
#'           sim_runs = 200)
#'
#' print(df_wo_subgroups)
#' ## Close the PSOCK cluster created with future
#' if (!inherits(future::plan(), "sequential")){future::plan("sequential")}
#'
#' # Simulated power, Type M, and Type S error
#' # for a conjoint design with 3 observed sub-groups of respondents.
#'df_w_subgroups <- power_sim(
#'           n_attributes = 3,
#'           n_levels = c(2, 3, 5),
#'           n_tasks = 3,
#'           group_name = c("Democrat", "Independent", "Republican"),
#'           units = c(500, 200, 500),
#'          true_coef = list("Democrat" = list(0.2, c(-0.1, 0.1), c(-0.1, -0.1, -0.1, 0.02)),
#'                           "Independent" = list(0.1, c(-0.2, -0.05),  c(-0.1, 0.1, 0.1, 0.3)),
#'                           "Republican" = list(0.1, c(-0.1, 0.01),  c(-0.1, 0.2, -0.1, 0.1))
#'                              ),
#'          sigma.u_k = 0.05,
#'          seed = 78,
#'          sim_runs = 200)
#'
#' print(df_w_subgroups, n=40)
#' ## Close the PSOCK cluster created with future
#'  if (!inherits(future::plan(), "sequential")){future::plan("sequential")}
#'
#'
#'

power_sim <- function(
    n_attributes,
    n_levels,
    group_name = NULL,
    units,
    n_tasks,
    true_coef,
    sigma.u_k,
    sim_runs = 100,
    seed,
    n_sim,
    p
){

  # Sanity checks for the input
  if(is.null(n_attributes)){stop("Input problem: n_attributes is missing!")}
  if(is.null(n_levels)){stop("Input problem: n_levels is missing!")}
  if(is.null(units)){stop("Input problem: units is missing!")}
  if(is.null(n_tasks)){stop("Input problem: n_tasks is missing!")}
  if(is.null(true_coef)){stop("Input problem: true_coef is missing!")}
  if(is.null(seed)){stop("Input problem: seed is missing!")}

  # specification of the progress bar for lapply
  progressr::handlers(list(
    progressr::handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 60,
      complete = "+"
    )
  ))

  # detect core in the machine
  future::plan("multisession", workers = parallel::detectCores() - 1)

  message("========== Parallel computation using ", parallel::detectCores() - 1, " cores for performance gain ==========")

  #  generate and evaluate the simulation using sim_cj() n_sim times
  progressr::with_progress({
    p <- progressr::progressor(along = 1:sim_runs)
    results <- future.apply::future_lapply(1:sim_runs, sim_cj,
                             n_attributes = n_attributes,
                             n_levels = n_levels,
                             n_tasks = n_tasks,
                             group_name = group_name,
                             units = units,
                             true_coef = true_coef,
                             sigma.u_k = sigma.u_k,
                             future.seed = seed,
                             prog=TRUE,
                             p = p
    )

  })

  # bind the results
  bin_sim <- do.call(rbind, results)
  # include the overall seed
  bin_sim$seed <- seed

  if(is.null(group_name)){
    summary <- bin_sim |> group_by(attrb, level, true_coef, seed) |>
      summarise_at(vars(sig_robust, in_ci_robust, typeS_robust, typeM_robust),list(~sd(.x, na.rm=TRUE), ~mean(.x, na.rm=TRUE) )) |>
      mutate(sig_robust_mean = paste0(format(round(sig_robust_mean,digits=2),nsmall=2), " (", format(round(sig_robust_sd,digits=2),nsmall=2), ")" ),
             in_ci95_robust_mean = paste0(format(round(in_ci_robust_mean,digits=2),nsmall=2), " (", format(round(in_ci_robust_sd,digits=2),nsmall=2), ")" ),
             typeS_robust_mean= paste0(format(round(typeS_robust_mean,digits=2),nsmall=2), " (",format(round(typeS_robust_sd,digits=2),nsmall=2), ")" ),
             typeM_robust_mean= paste0(format(round(typeM_robust_mean,digits=2),nsmall=2), " (", format(round(typeM_robust_sd,digits=2),nsmall=2), ")" )
      ) |>
      rename(`power` = sig_robust_mean,
             `typeS` = typeS_robust_mean,
             `typeM` = typeM_robust_mean) |>
      select(attrb, level, true_coef, power, typeS, typeM, seed) |>
      ungroup()
  }else{
    summary <- bin_sim |> group_by(id_grp, attrb, level, true_coef, seed) |>
      summarise_at(vars(sig_robust, in_ci_robust, typeS_robust, typeM_robust),list(~sd(.x, na.rm=TRUE),~mean(.x, na.rm=TRUE) )) |>
      mutate(sig_robust_mean = paste0(format(round(sig_robust_mean,digits=2),nsmall=2), " (", format(round(sig_robust_sd,digits=2),nsmall=2), ")" ),
             in_ci95_robust_mean = paste0(format(round(in_ci_robust_mean,digits=2),nsmall=2), " (", format(round(in_ci_robust_sd,digits=2),nsmall=2), ")" ),
             typeS_robust_mean= paste0(format(round(typeS_robust_mean,digits=2),nsmall=2), " (",format(round(typeS_robust_sd,digits=2),nsmall=2), ")" ),
             typeM_robust_mean= paste0(format(round(typeM_robust_mean,digits=2),nsmall=2), " (", format(round(typeM_robust_sd,digits=2),nsmall=2), ")" )
      ) |>
      rename(`power` = sig_robust_mean,
             `typeS` = typeS_robust_mean,
             `typeM` = typeM_robust_mean) |>
      select(id_grp, attrb, level, true_coef, power, typeS, typeM, seed) |>
      ungroup()
  }



  return(summary)

}

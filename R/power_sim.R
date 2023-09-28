#' Title
#'
#' @param n_attributes
#' @param n_levels
#' @param group_name
#' @param units
#' @param n_tasks
#' @param true_coef
#' @param sigma.u_k
#' @param sim_runs
#' @param seed
#' @param n_sim
#' @param p
#'
#' @return
#' @export
#'
#' @examples
#'
power_sim <- function(
    n_attributes = NULL,
    n_levels = NULL,
    group_name = NULL,
    units = NULL,
    n_tasks = NULL,
    true_coef= NULL,
    sigma.u_k = NULL,
    sim_runs = 100,
    seed = NULL,
    n_sim = NULL,
    p
){

  if(is.null(n_attributes)){stop("Input problem: n_attributes is missing!")}
  if(is.null(n_levels)){stop("Input problem: n_levels is missing!")}
  if(is.null(units)){stop("Input problem: units is missing!")}
  if(is.null(n_tasks)){stop("Input problem: n_tasks is missing!")}
  if(is.null(true_coef)){stop("Input problem: true_coef is missing!")}
  if(is.null(seed)){stop("Input problem: seed is missing!")}


  progressr::handlers(list(
    progressr::handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 60,
      complete = "+"
    )
  ))

  plan(multisession, workers = detectCores() - 1)

  with_progress({
    p <- progressor(along = 1:sim_runs)
    results <- future_lapply(1:sim_runs, sim_cj,
                             n_attributes = n_attributes,
                             n_levels = n_levels,
                             n_tasks = n_tasks,
                             group_name = group_name,
                             units = units,
                             true_coef = true_coef,
                             sigma.u_k = sigma.u_k,
                             future.seed = seed,
                             power=TRUE,
                             p = p
    )

  })

  bin_sim <- do.call(rbind, results)
  bin_sim$seed <- seed

  if(is.null(group_name)){
    summary <- bin_sim |> group_by(attrb, level, true_coef, seed) |>
      summarise_at(vars(sig, in_ci95, sig_robust, in_ci95_robust, typeS, typeS_robust, typeM, typeM_robust),list(~sd(.x, na.rm=TRUE),~mean(.x, na.rm=TRUE) )) |>
      mutate(in_ci95_robust_mean = paste0(format(round(in_ci95_robust_mean,digits=2),nsmall=2), " (", format(round(in_ci95_robust_sd,digits=2),nsmall=2), ")" ) ,
             typeS_robust_mean= paste0(format(round(typeS_robust_mean,digits=2),nsmall=2), " (",format(round(typeS_robust_sd,digits=2),nsmall=2), ")" ),
             typeM_robust_mean= paste0(format(round(typeM_robust_mean,digits=2),nsmall=2), " (", format(round(typeM_robust_sd,digits=2),nsmall=2), ")" )
      ) |>
      rename(`power` = in_ci95_robust_mean,
             `typeS` = typeS_robust_mean,
             `typeM` = typeM_robust_mean) |>
      select(attrb, level, true_coef, power, typeS, typeM, seed) |>
      ungroup()
  }else{
    summary <- bin_sim |> group_by(id_grp, attrb, level, true_coef, seed) |>
      summarise_at(vars(sig, in_ci95, sig_robust, in_ci95_robust, typeS, typeS_robust, typeM, typeM_robust),list(~sd(.x, na.rm=TRUE),~mean(.x, na.rm=TRUE) )) |>
      mutate(in_ci95_robust_mean = paste0(format(round(in_ci95_robust_mean,digits=2),nsmall=2), " (", format(round(in_ci95_robust_sd,digits=2),nsmall=2), ")" ) ,
             typeS_robust_mean= paste0(format(round(typeS_robust_mean,digits=2),nsmall=2), " (",format(round(typeS_robust_sd,digits=2),nsmall=2), ")" ),
             typeM_robust_mean= paste0(format(round(typeM_robust_mean,digits=2),nsmall=2), " (", format(round(typeM_robust_sd,digits=2),nsmall=2), ")" )
      ) |>
      rename(`power` = in_ci95_robust_mean,
             `typeS` = typeS_robust_mean,
             `typeM` = typeM_robust_mean) |>
      select(id_grp, attrb, level, true_coef, power, typeS, typeM, seed) |>
      ungroup()
  }

  return(summary)

}

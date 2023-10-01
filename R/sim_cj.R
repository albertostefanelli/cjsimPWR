#' Title
#'
#' @param n_attributes
#' @param n_levels
#' @param group_name
#' @param units
#' @param n_tasks
#' @param true_coef
#' @param sigma.u_k
#' @param n_sim
#' @param power
#' @param p
#'
#' @return
#' @export
#'
#' @examples
#'

sim_cj <- function(
    n_attributes = NULL,
    n_levels = NULL,
    group_name = NULL,
    units = NULL,
    n_tasks = NULL,
    true_coef= NULL,
    sigma.u_k = NULL,
    n_sim = NULL,
    power = TRUE,
    p
){

  if(is.null(n_attributes)){stop("Input problem: n_attributes is missing")}
  if(is.null(n_levels)){stop("Input problem: n_levels is missing")}
  if(is.null(units)){stop("Input problem: units is missing")}
  if(is.null(n_tasks)){stop("Input problem: n_tasks is missing")}
  if(is.null(true_coef)){stop("Input problem: true_coef is missing")}
  if(is.null(sigma.u_k)){stop("Input problem: sigma.u_k is missing")}


  if(is.null(group_name)){

    design <- generate_design(n_profiles = 2,
                              n_attributes = n_attributes,
                              n_levels = n_levels)

    sample <- generate_samples(design = list(design),
                               units = units,
                               n_tasks = n_tasks
    )

  }else{

    design <- generate_design(n_profiles = 2,
                              n_attributes = n_attributes,
                              n_levels = n_levels)


    sample <- generate_samples(design = rep(list(design),length(group_name)),
                               units = units,
                               n_tasks = rep(n_tasks,length(group_name)),
                               group_name = group_name
    )

  }

  simulated_cj <- simulate_conjoint(sample,
                                    true_coef = true_coef,
                                    sigma.u_k = sigma.u_k,
                                    LOG = FALSE)


  simulated_cj_long <- sim_to_long(simulated_cj)

  evaluate <- evaluate_model(simulated_cj_long, robust = TRUE)

  evaluate$sim <- n_sim

  if(power){p(sprintf("x=%g", n_sim))}else{}

  return(evaluate)

}


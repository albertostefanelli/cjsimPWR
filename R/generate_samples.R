
#' Generate samples from sub-populations
#'
#' @param design dataframe from generate_design()
#' @param units int; number of respondents
#' @param n_tasks int; number of tasks performed by each respondent
#' @param LOG debugging
#' @param group_name char; names for the unobserved populations
#'
#' @return samples from the full factorial for each sub populations n_population * units * n_tasks
#' @export
#'
#' @examples
#'

generate_samples <- function(design,         # data frame from generate_design()
                             units,          # number of respondents
                             n_tasks,        # number of tasks
                             LOG = FALSE,
                             group_name  = NULL      # name of the group
){

  # Generate sub-samples of profiles from the full factorial design
  # ---------------------------------------------------------------------------
  # Input: design - dataframe from generate_design()
  #        units - int; number of respondents
  #        n_tasks - int; number of tasks performed by each respondent
  #        group_name - char; names for the unobserved populations
  #
  # Output: dataframe - samples from the full factorial for each sub populations
  #                     n_population * units * n_tasks
  # ---------------------------------------------------------------------------


  if (length(design) == 1){
    if(LOG){message("========= Generating sample(s) for ", length(design), " population(s) =========")}
    li <- list(design, units, n_tasks)
    sb <- pmap(li, generate_sample) %>% reduce(rbind)

    output <- list(data = sb,
                   inputs = list(
                     units = units,
                     n_tasks = n_tasks
                   ))
  }else{
    if(LOG){message("========= Generating sample(s) for ", length(design), " population(s) =========")}
    li <- list(design, units, n_tasks, group_name)
    sb <- pmap(li, generate_sample) %>% reduce(rbind)

    output <- list(data = sb,
                   inputs = list(
                     units = units,
                     n_tasks = n_tasks,
                     group_name = group_name
                   ))
  }

  return(output)
}

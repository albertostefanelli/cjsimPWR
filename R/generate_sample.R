#' @description Generate sample of profiles from the full factorial design
#' @param design dataframe from generate_design()
#' @param units int; number of respondents
#' @param n_tasks int; number of tasks performed by each respondent
#' @param group_name character; vector of names for the observed populations
#'
#' @return  samples from the full factorial design in dimension  units * n_tasks
#' @export
#'
#' @examples
#'
#'
#'
generate_sample <- function(design,         # data frame from generate_design()
                            units,          # number of respondents
                            n_tasks,        # number of tasks
                            group_name = NULL      # name of the group

){


  if(is.null(group_name)){  sample <- dplyr::sample_n(design, size = units * n_tasks, replace = TRUE) %>%
    add_column(id = paste(group_name,
                          rep(1:units, each = n_tasks), sep = ''),
               .before = "Profile_1_var_1")
  }else{

    sample <- dplyr::sample_n(design, size = units * n_tasks, replace = TRUE) %>%
      add_column(id = paste(group_name,
                            rep(1:units, each = n_tasks), sep = ''),
                 .before = "Profile_1_var_1") %>%
      add_column(id_grp = group_name, .before = "id")
  }
}


as.character()

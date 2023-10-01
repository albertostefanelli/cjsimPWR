#' Generate samples of conjoint profiles drawing from the full factorial design
#' @description Sample of profiles from the full factorial design. If multiple (sub)populations are present, generate_sample should be repeated for each population
#' @param design single or a list of dataframe from generate_design()
#' @param units int; single or a list indicating the number of respondents
#' @param n_tasks int;  single or a list indicating number of tasks performed by each respondent
#' @param LOG logical; return debugging information on sample generation
#' @param group_name character; optional vector of names for the observed populations/subgroups of respondents
#'
#' @return Samples from the full factorial for each sub populations n_population * units * n_tasks
#' @export
#'
#' @examples
#'
#' design_example <- generate_design(n_profiles = 2,
#'  n_attributes = 3,
#'  n_levels = c(2, 3, 5))
#'
#' sample <- generate_samples(design = design_example,
#'                                 units = 100,
#'                                 n_tasks = 3)
#'
#' sample_usa <- generate_sample(design =  list(design_example, design_example, design_example),
#'                                units = c(500, 200, 500),
#'                                n_tasks = 3,
#'                                group_name = c("Democrat","Independent", "Republican" )
#'                                )
#'
#'
#'
#'


generate_samples <- function(design,
                             units,
                             n_tasks,
                             LOG = FALSE,
                             group_name  = NULL
){



  if(is.data.frame(design)){
    design <- list(design)
  }


  if(LOG){message("========= Generating sample(s) for ", length(design), " population(s) =========")}

  sample <- lapply(seq_along(design), function(d) {
    if(length(group_name)>1){


      df <- sample_n(design[[d]], size = units[[d]] * n_tasks[[d]], replace = TRUE) %>%
        add_column(id = paste(group_name[[d]],
                              rep(1:units[[d]], each = n_tasks[[d]]), sep = ''),
                   .before = "Profile_1_var_1") %>%
        add_column(id_grp = group_name[[d]], .before = "id")

      return(df)
    }else{

      df <- sample_n(design[[d]], size = units[[d]] * n_tasks[[d]], replace = TRUE) %>%
        add_column(id = paste(rep(1:units[[d]], each = n_tasks[[d]]), sep = ''),
                   .before = "Profile_1_var_1")

      return(df)

    }
  } ) %>% purr::reduce(rbind)

  return(sample)

}

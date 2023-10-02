#' Wide to long transformation of the simulated conjoint data
#' @description Transform the wide conjoint dataframe contained in simulate_conjoint() in a long one for analysis.
#' @param input list; A list containg a dataframe and inputs from simulate_conjoint() function.
#'
#' @return list; List composed of (a) a dataframe in a long format and  (b) the conjoint design specifications (inputs)
#' @export
#'
#' @examples
#' # Conjoint design with 3 attributes with 2, 3, 5 levels respectively
#' design_example <- generate_design(n_profiles = 2,
#'  n_attributes = 3,
#'  n_levels = c(2, 3, 5))
#'
#' # Design with multiple groups/sub-populations:
#' # random sample of 500 Democratic, 200 Independent, 500 Republican respondents
#' sample_subgrp <- generate_samples(design =  list(design_example, design_example, design_example),
#'                                units = c(500, 200, 500),
#'                                n_tasks = c(3, 3, 3),
#'                                group_name = c("Democrat","Independent", "Republican" )
#'                                )
#'
#' # Simulated data using the coefficients contained in true_coef for each subgroup of respondents
#' simulated_cj <- simulate_conjoint(sample_subgrp,
#'             true_coef = list("Democrat" = list(0.2, c(-0.1, 0.1), c(-0.1, -0.1, -0.1, 0.1)),
#'                              "Independent" = list(0.1, c(-0.2, -0.05),  c(-0.1, 0.1, 0.1, 0.3)),
#'                              "Republican" = list(0.1, c(-0.1, 0.05),  c(-0.1, 0.2, -0.1, 0.1))),
#'            sigma.u_k = 0.05,
#'            LOG = FALSE)
#'
#' # Transform the dataframe obtained from simulate_conjoint() in a long format
#' simulated_cj_long <- sim_to_long(simulated_cj)

sim_to_long <- function(input){
  data_final <- input$data


  # Add task indicator
  data_final <- data_final |>
    mutate(y2 = ifelse(y1 == 1, 0, 1)) |>
    group_by(id) |>
    mutate(task = seq(from = 1,
                      to = input$inputs$n_tasks[1],
                      by = 1))

  if (!"id_grp" %in% names(data_final)){grps_num <- 1}else{
    grps_num <- length(unique(data_final$id_grp))
  }

  if (grps_num == 1) {
    # Wide to long format
    # Split into two
    data_final |> ungroup() |> select(id, task, starts_with("Profile_1"), y1) -> p1
    data_final |> ungroup() |> select(id, task, starts_with("Profile_2"), y2) -> p2

    # Rename vars to match
    # Profile 1
    names(p1)[grep("Profile_", names(p1))] <- paste0("var_",
                                                     (1:(length(
                                                       grep("Profile_",
                                                            names(p1))))))
    names(p1)[grep("y1", names(p1))] <- "y"
    # Profile 2
    names(p2)[grep("Profile_",names(p2))] <- paste0("var_",
                                                    (1:(length(
                                                      grep("Profile_",
                                                           names(p2))))))
    names(p2)[grep("y2", names(p2))] <- "y"


    # Combine in one dataframe
    stack <- rbind(p1,p2) |>
      arrange(id, task) |>
      ungroup() |>
      mutate_at(vars(starts_with("var_")), factor)

    input$data <- stack

  }else{

    # Wide to long format
    # Split into two
    data_final |> ungroup() |> select(id_grp, id, task, starts_with("Profile_1"), y1) -> p1
    data_final |> ungroup() |> select(id_grp, id, task, starts_with("Profile_2"), y2) -> p2
    # Rename vars to match
    # Profile 1
    names(p1)[grep("Profile_", names(p1))] <- paste0("var_",
                                                     (1:(length(
                                                       grep("Profile_",
                                                            names(p1))))))
    names(p1)[grep("y1", names(p1))] <- "y"
    # Profile 2
    names(p2)[grep("Profile_",names(p2))] <- paste0("var_",
                                                    (1:(length(
                                                      grep("Profile_",
                                                           names(p2))))))
    names(p2)[grep("y2", names(p2))] <- "y"


    # Combine in one dataframe
    stack <- rbind(p1,p2) |>
      arrange(id, task) |>
      ungroup() |>
      mutate_at(vars(starts_with("var_")), factor) |>
      mutate_at(vars(id_grp), factor)

    input$data <- stack
  }
  return(input)
}

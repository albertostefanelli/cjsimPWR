#' Title
#' @description Transform the wide conjoint dataframe in a long one for analysis
#' @param input dataframe from simulate_conjoint() function
#'
#' @return a long dataframe
#' @export
#'
#' @examples
#'
#'
#'
sim_to_long <- function(input){
  data_final <- input$data
  #data_final <- simulated_cj$data


  # Add task indicator
  data_final <- data_final %>%
    mutate(y2 = ifelse(y1 == 1, 0, 1)) %>%
    # mutate(id_num = rep(seq(1, sum(input$inputs$units)),
    #                     each = input$inputs$n_tasks[1])) %>%
    group_by(id) %>%
    mutate(task = seq(from = 1,
                      to = input$inputs$n_tasks[1],
                      by = 1))

  if (!"id_grp" %in% names(data_final)){grps_num <- 1}else{
    grps_num <- length(unique(data_final$id_grp))
  }

  if (grps_num == 1) {
    # Wide to long format
    # Split into two
    data_final %>% ungroup() |> select(id, task, starts_with("Profile_1"), y1) -> p1
    data_final %>% ungroup() |> select(id, task, starts_with("Profile_2"), y2) -> p2

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
    stack <- rbind(p1,p2) %>%
      arrange(id, task) %>%
      ungroup() %>%
      mutate_at(vars(starts_with("var_")), factor)

    input$data <- stack

  }else{

    # Wide to long format
    # Split into two
    data_final %>% ungroup() |> select(id_grp, id, task, starts_with("Profile_1"), y1) -> p1
    data_final %>% ungroup() |> select(id_grp, id, task, starts_with("Profile_2"), y2) -> p2
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
    stack <- rbind(p1,p2) %>%
      arrange(id, task) %>%
      ungroup() %>%
      mutate_at(vars(starts_with("var_")), factor) %>%
      mutate_at(vars(id_grp), factor)

    input$data <- stack
  }
  return(input)
}

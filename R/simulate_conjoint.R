
#' @description Simulate conjoint process of profile selection
#' @param input dataframe with treatment variables generatedby generate_sample() - i.e. wide conjoint format
#' @param true_coef a vector/list of coefficients for all params to be estimated in order: var_1_lvl_1, var_1_lvl2, var_2_lvl1, var_2_lvl2,...
#' @param sigma.u_k standard deviation of the treatment effect for each variable (e.g, var_1_lvl_1, var_1_lvl2, var_2_lvl1, var_2_lvl2,...)
#' @param LOG returns parsing of coefficients and sanity checks
#'
#' @return
#' @export
#'
#' @examples
#'

simulate_conjoint <- function(input,
                              true_coef,
                              sigma.u_k = 0.02,
                              LOG = FALSE){

  # Simulate conjoint process of profile selection
  # ---------------------------------------------------------------------------
  # Input: data - dataframe with treatment variables generated
  #               by generate_sample() - i.e. wide conjoint format
  #        coef - a vector/list of coefficients for all params to be estimated
  #               in order: var_1_lvl_1, var_1_lvl2, var_2_lvl1, var_2_lvl2,...
  #        sigma.u_k - standard deviation of the treatment effect for each var
  #        LOG - used for diagnosing bugs in the function: prints checks
  #
  # Output: dataframe - addition of column "y" to data, meaning selection of
  #                     that respective profile - in long conjoint format
  # ---------------------------------------------------------------------------

  # design_example <- generate_design(n_profiles = 2,
  #                                   n_attributes = 2,
  #                                   n_levels = c(2, 6))
  #
  #
  # sample_subgrp <- generate_samples(design = list(design_example),
  #                                   units = 1000,
  #                                   n_tasks = 3)
  #
  #

  data <- input$data

  # Get inputs for later calculations
  num_trials <- input$inputs$n_tasks

  # Get number of variables in the conjoint design: num_vars
  num_vars <- length(names(select(data, starts_with("Profile_1"))))

  # Get number of levels for each variable in the conjoint design: num_lvls
  sim_lvls <- select(data, starts_with("Profile_1"))
  num_lvls <- c()
  for(i in 1:num_vars){
    num_lvls[i] <- length(names(table(
      eval(parse(text=(paste0("data$Profile_1_var_", i)))))
    ))
  }

  # Check that the names of the sub populations matche the named true_coef


  if (is.null(data$id_grp)){}else{
    if(!sum(unique(data$id_grp) == names(true_coef))==length(unique(data$id_grp))){stop("group_name and true_coef do not match.\n Maybe the generate_samples and simulate_conjoint population names are not the same?")}else{}
  }


  # Get number of groups: grps_num

  if (is.null(data$id_grp)){grps_num <- 1}else{
    grps_num <- length(unique(data$id_grp))
  }

  # Calculate number of parameters needed for the simulation
  num_parameters <- (sum(num_lvls) - num_vars) * grps_num
  #
  #
  # #### Run basic checks for inputs
  # Check if the input number of parameters is correct
  if(num_parameters > length(unlist(true_coef))){stop("Input problem: Not enough parameters! \n --> Expected ", num_parameters - 1, " true_coef (sum(num_lvls)*grps_num - 1)." )}
  if(num_parameters < length(unlist(true_coef))){stop("Input problem: Too many parameters! \n --> Expected ", num_parameters - 1, " true_coef (sum(num_lvls)*grps_num - 1)." )}


  # Get a list of length num_vars and vector of coefs for each (level - 1)
  if (grps_num == 1) {

    lvls_list <- list()
    var_list <- list()
    grp_list <- c()

    for (p in 1:grps_num){

      for(var in 1:num_vars){

        lvls_list <- c()
        for(lvl in 1:(num_lvls - 1)[var]){
          # write coefficients depending on the n level of an attribute (var)
          lvls_list[lvl] <- true_coef[[var]][[lvl]]

        }
        # write all the coefficients for a specific attribute (var)
        var_list[[var]] <- lvls_list

      }

      # write all the coefficients for a specific attribute for a specific population (p)
      grp_list[[p]] <- var_list

    }
    rm(lvls_list,var_list)


  }else{

    lvls_list <- list()
    var_list <- list()
    grp_list <- c()

    for (p in 1:grps_num){

      for(var in 1:num_vars){

        lvls_list <- c()
        for(lvl in 1:(num_lvls - 1)[var]){
          # write coefficients depending on the n level of an attribute (var)
          lvls_list[lvl] <- true_coef[[p]][[var]][[lvl]]

        }
        # write all the coefficients for a specific attribute (var)
        var_list[[var]] <- lvls_list

      }

      # write all the coefficients for a specific attribute for a specific population (p)
      grp_list[[p]] <- var_list

    }
    rm(lvls_list,var_list)

  }




  ### Check if coefficients were parsed correctly
  if(LOG){
    message("========= simulate_conjoint() diagnostic check =========")
    message("========================================================")
    message("= Parsing of coefficients:")
    if (grps_num == 1) {      message("========= No Subpopulations"," =========")
      for(var in 1:num_vars){
        coef_extracted <- paste(grp_list[[p]][[var]], sep = ", ")
        for(z in 1:length(coef_extracted)){
          message("= Variable ", var, " (levels: ", num_lvls[var],
                  "), dummy ", z, "/", num_lvls[var] - 1, ": ",
                  coef_extracted[z])
        }
      }
    }else{
      for (p in 1:grps_num){
        message("========= Subpopulation number ", p, " -- ", unique(data$id_grp)[[p]], " =========")
        for(var in 1:num_vars){
          coef_extracted <- paste(grp_list[[p]][[var]], sep = ", ")
          for(z in 1:length(coef_extracted)){
            message("= Variable ", var, " (levels: ", num_lvls[var],
                    "), dummy ", z, "/", num_lvls[var] - 1, ": ",
                    coef_extracted[z])
          }
        }
        message("========================================================")
      }}
  }




  if (grps_num == 1) {
    data[, paste0("probability_p1")] <- 0.5
    data[, paste0("probability_p2")] <- 0.5

    # Iterate over all variables and levels
    for(var in 1:num_vars){
      for(lvl in 1:(num_lvls - 1)[var]){
        #### Treatment effect heterogeneity for each trials
        # Generate vector of random effects for each variable * (lvls - 1)
        # and calculate the individual effect per each respondent:
        #     indiv_coef_x = true_coef_x + random_effect_u_x

        # Stage 1: Random effect generation
        # calculate a random effect for each variable/level
        reff_u <- paste0("raneff_u", var, "_lvl", lvl)

        data[, paste0(reff_u)] <- rep(rnorm(length(unique(data[, 'id'])),
                                            mean = 0, sd = sigma.u_k))



        # Stage 1: Effect calculation
        # Calculate respondent specific effect per each variable * (lvl - 1)
        coef_x <- paste0("cx", var, "_lvl", lvl)
        # [resp.-spec. coef]        [average effect]   +   [random effect respondent]
        data[, paste0(coef_x)] <- true_coef[[var]][[lvl]] + data[, paste0(reff_u)]

        #### Calculate the predicted probability of selecting a profile
        # Linear probability predictor
        data[,paste0("probability_p1")] <- data[, paste0("probability_p1")] +
          data[, paste0(coef_x)] * ifelse(data[, paste0("Profile_1_var_", var)] == lvl, 1, 0)

        # Debug message:
        if(LOG){
          message("= Multiplying: G", " = ", coef_x, " * ", "Profile_1_var_",
                  var, "(lvl = ", lvl, ")")
        }

        data[,paste0("probability_p2")] <- data[, paste0("probability_p2")] +
          data[, paste0(coef_x)] * ifelse(data[, paste0("Profile_2_var_", var)] == lvl, 1, 0)

      }
    }
  }else{
    # Stage 0 : Random effect generation

    for (p in 1:grps_num){
      # Filter the specific subgroup
      rows_pop <- data$id_grp == names(table(data$id_grp))[p]
      data[rows_pop, paste0("probability_p1")] <- 0.5
      data[rows_pop, paste0("probability_p2")] <- 0.5

      # Iterate over all variables and levels
      for(var in 1:num_vars){
        for(lvl in 1:(num_lvls - 1)[var]){
          #### Treatment effect heterogeneity for each trials
          # Generate vector of random effects for each variable * (lvls - 1)
          # and calculate the individual effect per each respondent:
          #     indiv_coef_x = true_coef_x + random_effect_u_x

          # Stage 1: Random effect generation
          # calculate a random effect for each variable/level
          reff_u <- paste0("raneff_u", var, "_lvl", lvl)

          data[rows_pop, paste0(reff_u)] <- rep(rnorm(length(unique(data[rows_pop, 'id'])),
                                                      mean = 0, sd = sigma.u_k),
                                                each = num_trials[p])




          # Stage 1: Effect calculation
          # Calculate respondent specific effect per each variable * (lvl - 1)
          coef_x <- paste0("cx", var, "_lvl", lvl)
          # [resp.-spec. coef]        [average effect]   +   [random effect respondent]
          data[rows_pop, paste0(coef_x)] <- true_coef[[p]][[var]][[lvl]] + data[rows_pop, paste0(reff_u)]

          #### Calculate the predicted probability of selecting a profile
          # Linear probability predictor
          data[rows_pop,paste0("probability_p1")] <- data[rows_pop, paste0("probability_p1")] +
            data[rows_pop, paste0(coef_x)] * ifelse(data[rows_pop, paste0("Profile_1_var_", var)] == lvl, 1, 0)

          # Debug message:
          if(LOG){
            message("= Multiplying: G", p, " = ", coef_x, " * ", "Profile_1_var_",
                    var, "(lvl = ", lvl, ")")
          }

          data[rows_pop,paste0("probability_p2")] <- data[rows_pop, paste0("probability_p2")] +
            data[rows_pop, paste0(coef_x)] * ifelse(data[rows_pop, paste0("Profile_2_var_", var)] == lvl, 1, 0)

        }
      }
    }
  }

  data$probability_p1 <- pmin(data$probability_p1, 0.999)
  data$probability_p1 <- pmax(data$probability_p1, 0.001)
  data$probability_p2 <- pmin(data$probability_p2, 0.999)
  data$probability_p2 <- pmax(data$probability_p2, 0.001)

  # Stage 2: Generate selections
  # selected_p1 <- c()
  # for(i in 1:dim(data)[1]){
  #   selected_p1[i] <- sample(x = c(1, 0), size = 1, replace = F,
  #                         prob = c(data$probability_p1[i],
  #                                  data$probability_p2[i]))
  # }

  #### Select the winning profile
  # Odds ratio of choosing P1 vs. P2
  # This is the fucking magic sauce I was looking for!
  odds_choose_p1 <- (data$probability_p1 / (1 - data$probability_p1)) /
    (data$probability_p2 / (1 - data$probability_p2))

  # Convert to probability
  prob_choose_p1 <- odds_choose_p1 / (1 + odds_choose_p1)

  # Generate selections with rbinom()
  selected_p1 <- rbinom(n = length(prob_choose_p1), size = 1,
                        prob = prob_choose_p1)

  # Gather everything into one dataframe
  data_final <- data.frame(data,
                           y1 = selected_p1)


  if (grps_num == 1) {  final <- list(data = data_final,
                                      inputs = list(
                                        units = input$inputs$units,
                                        n_tasks = input$inputs$n_tasks,
                                        true_coef = true_coef,
                                        sigma.u_k = sigma.u_k
                                      ))
  }else{
    final <- list(data = data_final,
                  inputs = list(
                    units = input$inputs$units,
                    n_tasks = input$inputs$n_tasks,
                    group_name = input$inputs$group_name,
                    true_coef = true_coef,
                    sigma.u_k = sigma.u_k
                  ))
  }
  return(final)
}

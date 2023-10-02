#' Generate full factorial experimental design.
#'
#' @description Generate full factorial experimental design based on the number of attributes (n_attributes) and number of levels (n_levels) included in the design.
#' @param n_attributes int; number of independent variables (i.e. treatments) in the conjoint experiment.
#' @param n_levels  int; vector of integers of length n_attributes; number of levels of variables should be > 1.
#' @param n_profiles int; number of profiles to be compared together. Always set to 2 as in Hainmueller, J., Hopkins, D., & Yamamoto, T. (2014) framework.
#' @param rem.eq.prof logical; remove comparisons of same profiles from the full factorial. This is relevant specifically for small design with small n_attributes and n_levels.
#'
#' @return dataframe; dataframe with a full factorial design, which combines all possible levels of the conjoint attributes attributes. The resulting dataframe can be passed to generate_samples()
#' @export
#'
#' @examples
#' # Conjoint design with 3 attributes with 2, 3, 5 levels respectively
#' design_example <- generate_design(n_profiles = 2,
#'  n_attributes = 3,
#'  n_levels = c(2, 3, 5)
#'  )
#'

generate_design <- function(n_attributes,
                            n_levels,
                            n_profiles = 2,
                            rem.eq.prof = TRUE){

  # Sanity check: test number of levels input
  if(length(n_levels) != n_attributes){
    stop("Number of levels is not equal to number of attributes.")
  }

  #Sanity check: test inputs for < 0
  if(n_profiles <= 0 |
     n_attributes <= 0){
    stop("Inputs have to be > 0.")
  }

  # Sanity check: test number of levels > 1
  if(any(n_levels <= 1)){
    stop("Number of levels has to be > 1.")
  }

  # Attribute names: var_1 ... var_x
  named_attributes <- rep(paste0("var_",1:n_attributes), each = n_profiles)
  named_attributes <- sort(named_attributes)

  # Profile names: Profile_1_var_1 ... Profile_2_var_x
  profile_names <- paste("Profile",1:(n_profiles), named_attributes, sep="_")

  # Levels of each var level_1 ... level_x
  att_levels <- rep(n_levels, 1, each = n_profiles)

  # list of all possible attribute-levels combinations for each profile
  grid_list <- c()
  for (i in 1:length(profile_names)){
    grid_list[[profile_names[i]]] <- as.numeric(0:(att_levels[i] - 1))
  }

  # list of all possible attribute-levels combinations
  design <- expand.grid(grid_list, stringsAsFactors = FALSE)

  if(rem.eq.prof){
    # Split profile_1 and profile_2
    p1 <- select(design, starts_with("Profile_1"))
    p2 <- select(design, starts_with("Profile_2"))

    # check if profile attribute-levels is the same
    same_profile <- apply((p1 == p2),
                          1, function(x) all(x))

    # if equal, remove equal profile
    design <- design[!same_profile, ]
  }

  return(design)
}

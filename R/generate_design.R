#' Generate design
#'
#' @description Generate full factorial experimental design
#' @param n_attributes int; number of independent variables (ie treatments) in the conjoint experiment
#' @param n_levels  int; vector of ints of length n_attributes; number of levels of variables should be > 1.
#' @param n_profiles int; number of profiles to be compared together. always set to 2
#' @param rem.eq.prof logical; remove comparisons of same profiles
#'
#' @return dataframe - large dataframe with a full factorial design, which combines all possible levels together.
#' @export
#'
#' @examples

generate_design <- function(n_attributes,
                            n_levels,
                            n_profiles = 2,
                            rem.eq.prof = T){

  ### A. Test number of levels input
  if(length(n_levels) != n_attributes){
    stop("Number of levels is not equal to number of attributes.")
  }

  ### B. Test inputs for < 0
  if(n_profiles <= 0 |
     n_attributes <= 0){
    stop("Inputs have to be > 0.")
  }

  ### C. Test number of levels > 1
  if(any(n_levels <= 1)){
    stop("Number of levels has to be > 1.")
  }

  # Attribute names: var_1 ... var_x
  named_attributes <- rep(paste0("var_",1:n_attributes), each = n_profiles)
  named_attributes <- sort(named_attributes)

  # Profile names: Profile_1_var_1 ... Profile_2_var_x
  profile_names <- paste("Profile",1:(n_profiles),named_attributes,sep="_")

  # Levels of each var
  att_levels <- rep(n_levels, 1, each = n_profiles)

  grid_list <- c()
  for (i in 1:length(profile_names)){
    grid_list[[profile_names[i]]] <- as.numeric(0:(att_levels[i] - 1))
  }


  design <- expand.grid(grid_list, stringsAsFactors = FALSE)


  if(rem.eq.prof){
    # Split
    p1 <- select(design, starts_with("Profile_1"))
    p2 <- select(design, starts_with("Profile_2"))

    # Check
    same_profile <- apply((p1 == p2),
                          1, function(x) all(x))

    # Remove
    design <- design[!same_profile, ]
  }

  return(design)
}

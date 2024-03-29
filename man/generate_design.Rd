% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_design.R
\name{generate_design}
\alias{generate_design}
\title{Generate full factorial experimental design.}
\usage{
generate_design(n_attributes, n_levels, n_profiles = 2, rem.eq.prof = TRUE)
}
\arguments{
\item{n_attributes}{int; number of independent variables (i.e. treatments) in the conjoint experiment.}

\item{n_levels}{int; vector of integers of length n_attributes; number of levels of variables should be > 1.}

\item{n_profiles}{int; number of profiles to be compared together. Always set to 2 as in Hainmueller, J., Hopkins, D., & Yamamoto, T. (2014) framework.}

\item{rem.eq.prof}{logical; remove comparisons of same profiles from the full factorial. This is relevant specifically for small design with small n_attributes and n_levels.}
}
\value{
The function returns a with a full factorial design, which combines all possible levels of the conjoint attributes attributes. The resulting dataframe can be passed to generate_samples()
}
\description{
Generate full factorial experimental design based on the number of attributes (n_attributes) and number of levels (n_levels) included in the design.
}
\examples{
# Conjoint design with 3 attributes with 2, 3, 5 levels respectively
design_example <- generate_design(n_profiles = 2,
 n_attributes = 3,
 n_levels = c(2, 3, 5)
 )

}

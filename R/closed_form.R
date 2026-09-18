# Closed-form power, Type S and Type M error for an unbiased, normally distributed estimate.
#
# Adapted from retro_design_closed_form.numeric() in the retrodesign package, version 0.2.2
# (https://CRAN.R-project.org/package=retrodesign), Copyright (c) 2018 Andrew G. Timm, MIT License
# (see inst/COPYRIGHTS). The formulas are the closed forms of Lu, Qiu and Deng (2019).
# Changes: vectorised over `truth` and `std.error`; Type S and Type M are NA when the true effect is zero.
#
# truth      true effect(s)
# std.error  standard error(s) of the estimate
# alpha      significance level of the two-sided test
# Returns a list with vectors power, type_s and type_m.
closed_form_measures <- function(truth, std.error, alpha = 0.05) {
  if (any(std.error < 0, na.rm = TRUE)) {
    stop("standard errors must not be negative", call. = FALSE)
  }
  A <- truth
  s <- std.error
  z <- stats::qnorm(1 - alpha / 2)
  p.hi <- 1 - stats::pnorm(z - A / s)
  p.lo <- stats::pnorm(-z - A / s)
  power <- p.hi + p.lo
  typeS <- ifelse(A >= 0, p.lo / power, 1 - (p.lo / power))
  lambda <- A / s
  typeM <- (stats::dnorm(lambda + z) + stats::dnorm(lambda - z) +
              lambda * (stats::pnorm(lambda + z) + stats::pnorm(lambda - z) - 1)) /
    (lambda * (1 - stats::pnorm(lambda + z) + stats::pnorm(lambda - z)))
  zero <- A == 0
  list(power = power,
       type_s = ifelse(zero, NA_real_, typeS),
       type_m = ifelse(zero, NA_real_, abs(typeM)))
}

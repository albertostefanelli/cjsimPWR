# Fixed-budget population references for latent utility models. Reference streams are
# allocated independently of analyses, shortcuts, optional AMCEs and experiment streams.
truth_controls <- function(control = list()) {
  keep <- c("seed", "exact_max_pairs", "exact_max_rows", "verification_pairs",
            "verification_draws", "verification_batches")
  defaults <- c(dgp_controls()[keep], list(paired_cells = 32768L, exact_max_cells = 5000000L))
  if (!is.list(control) || (length(control) && (is.null(names(control)) ||
      anyNA(names(control)) || anyDuplicated(names(control)) || any(!names(control) %in% names(defaults))))) {
    stop("unknown or unnamed truth control.", call. = FALSE)
  }
  defaults[names(control)] <- control
  for (nm in names(defaults)) {
    x <- defaults[[nm]]
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 ||
        x != round(x) || x > .Machine$integer.max / 4) {
      stop("truth control '", nm, "' must be a positive supported whole number.", call. = FALSE)
    }
  }
  if (defaults$verification_batches < 4 || defaults$verification_draws < 2) {
    stop("truth integration needs at least 4 verification batches and 2 draws.", call. = FALSE)
  }
  defaults
}

#' Simulate one conjoint experiment
#'
#' Draws the tasks of a paired conjoint experiment and simulates each respondent's choices.
#'
#' @param design a `cj_design` object from `conjoint_design()`.
#' @param true_amce AMCEs of the non-reference levels: a list with one numeric vector per attribute, matched
#'   to the attributes by name when named and by position otherwise. With groups, either one such list for
#'   all groups or a list of them named by group. Named coefficient vectors are matched to the design's
#'   non-reference level labels; unnamed vectors follow the design's level order.
#' @param units number of respondents: one number, or one per group (matched by name when named).
#' @param n_tasks number of tasks per respondent: one number, or one per group.
#' @param groups `NULL`, or the names of the respondent groups.
#' @param sigma heterogeneity of preferences: the standard deviation, across respondents, of each
#'   respondent's own AMCE, on the probability scale (`sigma = 0.05` means individual AMCEs spread
#'   with SD 0.05 around the requested AMCE). Available for the logit model; the linear model requires
#'   `sigma = 0`. A requested zero AMCE stays zero in the population; individual respondents can still
#'   have positive or negative effects under heterogeneity.
#'   There is no generally valid value: use pilot evidence where available, otherwise compare labelled
#'   scenarios such as `sigma = c(0, 0.05, 0.10, 0.15)` with the requested AMCEs held fixed.
#' @param dgp choice model: `"logit"` (default), calibrated so that generated AMCEs match `true_amce`
#'   within the calibration tolerance;
#'   `"linear"`, where the AMCEs equal `true_amce` exactly but no heterogeneity is possible; or
#'   `"odds"`, the deprecated model of versions up to 0.2.1, whose inputs are score coefficients rather
#'   than AMCEs.
#' @param latent_sigma alternative to `sigma`: the SD of respondent-level deviations on the model's
#'   coefficient scale. With `"odds"` it reproduces the old `sigma.u_k`. The implied AMCE SDs are
#'   reported in the model's `truth` table.
#' @param model optional prepared `cj_dgp` object, from a previous simulation's `dgp` attribute or
#'   [power_sim()]'s `model`, for repeated experiments without recalibration.
#'   Supply the same design and omit true_amce, sigma, dgp, latent_sigma and calibration_control.
#' @param calibration_control optional named list of calibration settings; see the "Calibration" section.
#'   `reference_margin = 0.5` enables experimental precision planning; its default of zero
#'   retains the original calibration and reference budgets.
#'
#' @section Calibration: For the logit model the package solves for coefficients whose AMCEs approximate
#'   `true_amce` (and, with `sigma > 0`, whose respondent-level AMCEs have SD `sigma`), then verifies the
#'   result with fresh reference draws. Verification stops when every true AMCE is within `tolerance`
#'   (default 0.001) of its request, with 99% Monte Carlo confidence, and every AMCE SD within
#'   `min(0.005, 0.05 * sigma)`; otherwise the calibration stops with an error. Designs with at most
#'   `exact_max_pairs` (default 10,000) profile pairs are enumerated exactly; larger designs are
#'   integrated by Monte Carlo. With `sigma > 0`, calibration takes seconds for small designs but can
#'   take many minutes when profiles must be sampled. The
#'   remaining settings (`seed`, `maxit`, `solver_tol`, `calibration_pairs`, `calibration_draws`,
#'   `verification_pairs`, `verification_draws`, `verification_batches`, `max_verification_batches`,
#'   `max_attempts`) control the solver and the size of the reference draws; their defaults are listed
#'   in `attr(data, "dgp")$control`. Calibration uses its own random seed and leaves the caller's random
#'   numbers unchanged.
#'   After acceptance, a fresh reference uses the accepted verification's integration settings and,
#'   by default, its number of batches. It is never retried or used to recalibrate. `diagnostics[[g]]$verification`
#'   records acceptance; `$reference` records the final estimate used in `truth`. A recheck that
#'   overlaps a tolerance boundary is quietly retained with `accepted = FALSE`. A warning is issued
#'   only when an interval lies wholly outside its target's tolerance band: `abs(estimate - target)`
#'   minus its half-width exceeds the tolerance. Then `contradicted = TRUE`; the model and reference
#'   are retained. Exact integration needs no fresh draws. Separately calibrated group contrasts match their requests within `tolerance`
#'   by verifying each group's AMCE within half that tolerance; they are not forced to zero.
#'
#'   Experimental precision planning is enabled with `reference_margin = m`, where `0 < m < 1`
#'   (suggested starting value 0.5; default 0 disables it). Verification then requires each estimated
#'   gap plus its margin to be at most `(1 - m)` times its AMCE or SD tolerance. The final reference
#'   retains the original tolerances. Before its draws, verification batch variances, inflated by a
#'   factor of two, determine the smallest batch count predicting half-widths at most `m * tolerance / 2`
#'   for every targeted quantity. Integration settings per batch stay fixed. The minimum batch count
#'   is `verification_batches`; `max_reference_batches` (default 512) caps the final count per group.
#'   `diagnostics[[g]]$reference_plan` records `batches`, `cap_limited`, `predicted_precision_met`,
#'   realised `precision_met`, and a `quantities` table with targets, predictions and actual half-widths.
#'   A cap or missed width target is diagnostic, not itself a warning or a reason to redraw. The
#'   inflation is a planning allowance, not a variance confidence bound; confirmation is not guaranteed.
#'   Stricter verification can take longer or exhaust its budget. Exact references need zero batches.
#'   This option requires AMCE targets and is unavailable for the legacy odds model.
#'
#'   The deprecated odds model has fixed scores rather than AMCE/SD targets. On each precision retry,
#'   it doubles `verification_pairs` and `verification_draws`, up to `max_attempts`, without changing
#'   the tolerance or drawing unused training samples. Exhausting this budget stops with a reference
#'   precision error. Its final `accepted` flag records precision only; `contradicted` is FALSE because
#'   there are no AMCE/SD targets to test. The separate null-sensitivity warning in [power_sim()] is unchanged.
#'
#' @return A data frame with one row per profile and columns `group` (`NA` without groups), `respondent`
#'   (unique across groups), `task`, `profile` (1 or 2), `y` (1 if the profile was chosen) and one factor
#'   per attribute, named as in the design. Attribute `dgp` contains the prepared model, including
#'   the true AMCEs, calibrated parameters and calibration diagnostics.
#'   Calibration/reference calculations preserve the caller's RNG; sampling the experiment advances it.
#' @export
#' @md
#' @examples
#' design <- conjoint_design(c(2, 3))
#' data <- simulate_experiment(design, list(0.05, c(-0.05, 0.1)), units = 100, n_tasks = 3)
#' head(data)
#' attr(data, "dgp")$truth
#'
#' # heterogeneous preferences: individual AMCEs have SD 0.05 around the requested values
#' mixed <- simulate_experiment(design, list(0.05, c(-0.05, 0.1)), units = 100, n_tasks = 3,
#'                              sigma = 0.05)
#' attr(mixed, "dgp")$truth[, c("attribute", "level", "true_amce", "amce_sd")]
#'
#' # reuse a calibrated model for another experiment
#' second <- simulate_experiment(design, units = 100, n_tasks = 3, model = attr(data, "dgp"))
simulate_experiment <- function(design, true_amce = NULL, units, n_tasks, groups = NULL, sigma = 0,
                                dgp = c("logit", "linear", "odds"), latent_sigma = NULL,
                                model = NULL, calibration_control = list()) {
  if (!inherits(design, "cj_design")) {
    stop("`design` must be created with conjoint_design().", call. = FALSE)
  }
  if (!is.null(model)) {
    if (!inherits(model, "cj_dgp") || !identical(model$design, design)) {
      stop("`model` must be a prepared DGP for this exact design.", call. = FALSE)
    }
    if ((!missing(true_amce) && !is.null(true_amce)) || !missing(sigma) || !missing(dgp) ||
        !missing(latent_sigma) || !missing(calibration_control)) {
      stop("with a prepared `model`, omit true_amce, sigma, dgp, latent_sigma and calibration_control.", call. = FALSE)
    }
    if (is.null(groups)) groups <- model$groups
    if (!identical(groups, model$groups)) stop("`groups` must match the prepared model's order.", call. = FALSE)
  }
  groups <- check_groups(groups)
  units <- per_group(units, groups, "units")
  n_tasks <- per_group(n_tasks, groups, "n_tasks")
  if (is.null(model)) {
    model <- prepare_dgp(design, true_amce, groups, sigma, dgp = match.arg(dgp),
                         latent_sigma = latent_sigma, control = calibration_control)
  }

  group_of <- rep(seq_along(units), times = units)  # group index of each respondent
  data <- sample_tasks(design, n_tasks[group_of])
  if (!is.null(groups)) {
    data$group <- groups[group_of[data$respondent]]
  }

  deviations <- matrix(stats::rnorm(length(group_of) * ncol(model$gamma)), nrow = length(group_of))
  coef_respondent <- model$gamma[group_of, , drop = FALSE] + deviations * model$raw_sd[group_of, , drop = FALSE]
  if (any(model$baseline_sd > 0)) {
    base <- matrix(stats::rnorm(length(group_of) * ncol(model$baseline_sd)), nrow = length(group_of)) *
      model$baseline_sd[group_of, , drop = FALSE]
    attribute <- rep(seq_along(design$n_levels), design$n_levels - 1)
    coef_respondent <- coef_respondent - base[, attribute, drop = FALSE]
  }
  score <- profile_scores(data, design, coef_respondent)
  first <- data$profile == 1L
  p1 <- switch(model$dgp,
               logit = stats::plogis(score[first] - score[!first]),
               linear = 0.5 + score[first] - score[!first],
               odds = odds_choice(score[first], score[!first]))
  y1 <- as.integer(stats::runif(length(p1)) < p1)
  data$y[first] <- y1
  data$y[!first] <- 1L - y1
  attr(data, "dgp") <- model
  data
}

# Draw the profiles of every task: one row per profile, ordered by respondent, task and profile.
# `tasks` gives the number of tasks of each respondent.
sample_tasks <- function(design, tasks) {
  respondent <- rep(seq_along(tasks), times = tasks)
  task <- sequence(tasks)
  n <- length(respondent)
  p1 <- draw_profiles(design$n_levels, n)
  p2 <- draw_profiles(design$n_levels, n)
  rows <- rep(seq_len(n), each = 2)
  profile <- rep(1:2, times = n)
  codes <- p1[rows, , drop = FALSE]
  codes[profile == 2L, ] <- p2
  data <- data.frame(group = NA_character_, respondent = respondent[rows], task = task[rows],
                     profile = profile, y = NA_integer_, stringsAsFactors = FALSE)
  attrs <- names(design$levels)
  for (k in seq_along(attrs)) {
    data[[attrs[k]]] <- structure(codes[, k], levels = design$levels[[k]], class = "factor")
  }
  data
}

# Level codes (1 = reference level) of n profiles drawn uniformly: one row per profile.
draw_profiles <- function(n_levels, n) {
  matrix(vapply(n_levels, function(l) sample.int(l, n, replace = TRUE), integer(n)), nrow = n)
}

# Sum of each profile's coefficients, using the coefficients of its respondent (one row per respondent,
# one column per non-reference level in design order).
profile_scores <- function(data, design, coef_respondent) {
  n_levels <- design$n_levels
  offsets <- cumsum(c(0, n_levels[-length(n_levels)] - 1))
  score <- numeric(nrow(data))
  attrs <- names(design$levels)
  for (k in seq_along(attrs)) {
    code <- as.integer(data[[attrs[k]]])
    has <- code > 1L
    score[has] <- score[has] + coef_respondent[cbind(data$respondent[has], offsets[k] + code[has] - 1L)]
  }
  score
}

# v0.2.1 choice model: each profile's score is 0.5 plus its coefficients, clipped to [0.001, 0.999];
# profile 1 is chosen with probability odds1 / (odds1 + odds2).
odds_choice <- function(a, b) {
  s1 <- pmin(pmax(0.5 + a, 0.001), 0.999)
  s2 <- pmin(pmax(0.5 + b, 0.001), 0.999)
  odds <- (s1 / (1 - s1)) / (s2 / (1 - s2))
  odds / (1 + odds)
}

# Group names, checked.
check_groups <- function(groups) {
  if (is.null(groups)) return(NULL)
  if (!is.character(groups) || length(groups) == 0 || anyNA(groups) || any(groups == "") ||
      anyDuplicated(groups)) {
    stop("`groups` must be unique, non-empty names.", call. = FALSE)
  }
  groups
}

# One positive whole number per group (or a single one without groups), matched by name when named.
per_group <- function(x, groups, arg) {
  n_groups <- max(1, length(groups))
  valid <- is.numeric(x) && length(x) %in% c(1, n_groups) && all(is.finite(x)) && all(x >= 1) &&
    all(x <= .Machine$integer.max) &&
    all(x == round(x))
  if (!valid) {
    stop("`", arg, "` must be one positive whole number", if (!is.null(groups)) ", or one per group", ".",
         call. = FALSE)
  }
  if (length(x) == 1) return(rep(as.integer(x), n_groups))
  if (!is.null(names(x))) {
    if (!setequal(names(x), groups)) {
      stop("names of `", arg, "` must match `groups`.", call. = FALSE)
    }
    x <- x[groups]
  }
  as.integer(unname(x))
}

# AMCEs as a matrix: one row per group (a single row without groups), one column per non-reference level
# in design order.
amce_matrix <- function(true_amce, design, groups = NULL) {
  per_group_input <- is.list(true_amce) && length(true_amce) > 0 && all(vapply(true_amce, is.list, logical(1)))
  if (is.null(groups)) {
    if (per_group_input) {
      stop("`true_amce` is given per group, but `groups` is missing.", call. = FALSE)
    }
    return(matrix(amce_vector(true_amce, design, ""), nrow = 1))
  }
  if (!per_group_input) {
    rows <- rep(list(amce_vector(true_amce, design, "")), length(groups))
  } else {
    if (length(true_amce) != length(groups)) {
      stop("`true_amce` must have one entry per group.", call. = FALSE)
    }
    if (!is.null(names(true_amce))) {
      if (!setequal(names(true_amce), groups)) {
        stop("names of `true_amce` do not match `groups`: ", paste(groups, collapse = ", "), ".", call. = FALSE)
      }
      true_amce <- true_amce[groups]
    }
    rows <- lapply(seq_along(groups), function(i) {
      amce_vector(true_amce[[i]], design, paste0(" for group '", groups[i], "'"))
    })
  }
  do.call(rbind, rows)
}

# AMCEs of one group as a vector in design order.
amce_vector <- function(amce, design, where) {
  attrs <- names(design$levels)
  if (is.numeric(amce) && length(amce) == length(attrs) && all(design$n_levels == 2)) {
    amce <- as.list(amce)
  }
  if (!is.list(amce) || length(amce) != length(attrs)) {
    stop("`true_amce`", where, " must be a list with one entry per attribute (", length(attrs), ").",
         call. = FALSE)
  }
  if (!is.null(names(amce))) {
    if (!setequal(names(amce), attrs)) {
      stop("names of `true_amce`", where, " do not match the attributes: ", paste(attrs, collapse = ", "), ".",
           call. = FALSE)
    }
    amce <- amce[attrs]
  }
  unlist(lapply(seq_along(attrs), function(k) {
    labels <- design$levels[[k]][-1]
    v <- amce[[k]]
    if (!is.numeric(v) || length(v) != length(labels) || !all(is.finite(v))) {
      stop("`true_amce`", where, " for attribute '", attrs[k], "' must have ", length(labels),
           " finite value(s), one per non-reference level.", call. = FALSE)
    }
    if (!is.null(names(v))) {
      if (!setequal(names(v), labels)) {
        stop("names of `true_amce`", where, " for attribute '", attrs[k], "' must be its non-reference levels: ",
             paste(labels, collapse = ", "), ".", call. = FALSE)
      }
      v <- v[labels]
    }
    unname(v)
  }))
}

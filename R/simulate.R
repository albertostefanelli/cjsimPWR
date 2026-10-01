#' Simulate one conjoint experiment
#'
#' Draws the tasks of a paired conjoint experiment and simulates each respondent's choices.
#'
#' @param design a `cj_design` object from `conjoint_design()`.
#' @param true_amce AMCEs of the non-reference levels: a list with one numeric vector per attribute, matched
#'   to the attributes by name when named and by position otherwise. With groups, either one such list for
#'   all groups or a list of them named by group. Named coefficient vectors are matched to the design's
#'   non-reference level labels; unnamed vectors follow the design's level order.
#' @param units positive whole number of respondents: one number, repeated for every group, or one per
#'   group, matched by name when named and by group order otherwise.
#' @param n_tasks positive whole number of tasks per respondent: one number, repeated for every group, or
#'   one per group, matched by name when named and by group order otherwise.
#' @param groups `NULL`, or the names of the respondent groups.
#' @param sigma heterogeneity of preferences: a single non-negative number, the standard deviation,
#'   across respondents, of each respondent's own AMCE, on the probability scale (`sigma = 0.05` means
#'   individual AMCEs spread with SD 0.05 around the requested AMCE). One call takes one `sigma`, shared
#'   across effects and groups; it does not accept a vector. A requested zero AMCE stays zero in the
#'   population; individual respondents can still have positive or negative effects under heterogeneity.
#'   There is no generally valid value: use pilot evidence where available, otherwise compare separate
#'   calls at labelled candidate values (for example, 0, 0.05, 0.10 and 0.15) with the requested AMCEs
#'   held fixed.
#' @param dgp choice model: `"logit"` (the default), calibrated so that generated AMCEs match
#'   `true_amce` within the calibration tolerance. The argument is retained so that later versions can
#'   add other choice models without changing existing calls.
#' @param latent_sigma alternative to `sigma`: the SD of respondent-level deviations on the logit
#'   coefficient scale. The implied AMCE SDs are reported in the model's `truth` table.
#' @param model optional prepared `cj_dgp` object, from a previous simulation's `dgp` attribute or
#'   [power_sim()]'s `model`, for repeated experiments without recalibration.
#'   Supply the same design and omit true_amce, sigma, dgp, latent_sigma and calibration_control.
#' @param calibration_control optional named list of calibration settings; see the Calibration section
#'   of [simulate_experiment()] below. `reference_margin = 0.5` enables experimental precision planning;
#'   its default of zero retains the original calibration and reference budgets.
#'
#' @section Calibration: The package solves for coefficients whose AMCEs approximate
#'   `true_amce` (and, with `sigma > 0`, whose respondent-level AMCEs have SD `sigma`), then verifies the
#'   solution against fresh reference draws at a nominal 99% Monte Carlo confidence. Verification stops
#'   when every true AMCE is within `tolerance` of its request and every AMCE SD is within
#'   `min(0.005, 0.05 * sigma)`; otherwise calibration stops with an error. Exact integration concerns
#'   profile combinations: equivalent differences between profiles are combined and opposite differences
#'   are folded into one weighted row. Integration is exact when the active design has at most
#'   `exact_max_pairs` ordered pairs and at most `exact_max_rows` folded rows, subject to a 256 MiB
#'   budget for the reference and work matrices (not total R process memory), checked before allocation.
#'   Larger designs integrate over sampled profiles; `exact_max_pairs = 1` always forces sampling.
#'   With heterogeneity, respondent coefficients still require Monte Carlo integration.
#'   The wider exact eligibility in version 0.3.0 changes seeded results and calibration random-number
#'   use for newly eligible designs; previously exact designs change only up to rounding.
#'   Calibration uses its own random seed
#'   (`calibration_control$seed`) and leaves the caller's random numbers unchanged.
#'
#'   After acceptance, a fresh, independent reference — never retried or used to recalibrate — supplies
#'   the true AMCEs, SDs and their Monte Carlo precision in `truth`. `diagnostics[[g]]$verification`
#'   records acceptance; `diagnostics[[g]]$reference` records this final estimate. Three outcomes follow:
#'   a recheck fully inside its tolerance band is accepted; one that overlaps the boundary is retained
#'   quietly with `accepted = FALSE` (inconclusive); one that lies wholly outside its band
#'   (`abs(estimate - target) - half-width > tolerance`) is retained with `contradicted = TRUE` and a
#'   warning. Separately calibrated group contrasts match their requests within `tolerance` by verifying
#'   each group's AMCE within half that tolerance; they are not forced to zero.
#'
#'   **Controls.** `calibration_control` accepts these named settings (defaults from `dgp_controls()`,
#'   also listed in `attr(data, "dgp")$control`):
#'
#'   | Setting | Default | Role |
#'   | --- | ---: | --- |
#'   | `seed` | 104729 | Solver: calibration's own random seed, independent of the caller's RNG. |
#'   | `maxit` | 100 | Solver: maximum solver iterations. |
#'   | `solver_tol` | 1e-6 | Solver: solver convergence tolerance. |
#'   | `tolerance` | 0.001 | Solver: verification tolerance for AMCE and SD targets. |
#'   | `exact_max_pairs` | 100000000 | Integration: at most this many ordered profile pairs for exact integration; the row and memory limits also apply. |
#'   | `exact_max_rows` | 20000 | Integration: at most this many folded profile-difference rows for exact integration. |
#'   | `calibration_pairs` | 16384 | Integration: sampled profile pairs per solver iteration (when not exact). |
#'   | `calibration_draws` | 2048 | Integration: respondent draws per profile pair while solving. |
#'   | `verification_pairs` | 32768 | Integration: profile pairs sampled per verification batch. |
#'   | `verification_draws` | 1024 | Integration: respondent draws per profile pair per batch. |
#'   | `verification_batches` | 16 | Integration: batches per verification attempt; also the minimum reference batch count. |
#'   | `max_verification_batches` | 128 | Integration: batch cap for one verification attempt. |
#'   | `max_attempts` | 4 | Integration: verification attempts allowed before calibration stops with an error. |
#'   | `reference_margin` | 0 | Precision planning: see below. |
#'   | `max_reference_batches` | 512 | Precision planning: cap on the final reference's batch count per group. |
#'
#'   **Precision planning (experimental).** `reference_margin` (`0 <= reference_margin < 1`) reserves
#'   verification headroom and plans the final reference's batch count from the verification variances.
#'   The default of 0 disables the option and retains the original calibration and reference budgets; a
#'   positive value (suggested starting point 0.5) can increase calibration runtime or cause stricter
#'   verification to fail. `max_reference_batches` caps the final budget per group.
#'   `diagnostics[[g]]$reference_plan` reports `batches`, `cap_limited`, `predicted_precision_met` and
#'   realised `precision_met`. A batch cap or a missed precision target alone does not raise a warning,
#'   and meeting the target is not guaranteed. See the
#'   [calibration guide](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/calibration.md)
#'   (online) for the full derivation and reading guide.
#'
#' @return A data frame with one row per profile and columns `group` (`NA` without groups), `respondent`
#'   (unique across groups), `task`, `profile` (1 or 2), `y` (1 if the profile was chosen) and one factor
#'   per attribute, named as in the design. Attribute `dgp` contains the prepared model (class `cj_dgp`),
#'   including:
#'   * `truth`: one row per effect, with `attribute`, `level` (and `group`/`reference_group` with groups),
#'     `requested_amce`, `true_amce` (the numerical reference), `reference_mcse`, `reference_half_width`,
#'     `amce_sd` (implied SD under heterogeneity), `sd_mcse` and `sd_half_width`. Subgroup-difference rows
#'     have `amce_sd`, `sd_mcse` and `sd_half_width` equal to `NA`. This table has no `effect_id` column;
#'     that identifier is added by [power_sim()].
#'   * `parameters`, `diagnostics`: calibrated latent parameters and the per-group calibration diagnostics
#'     described above.
#'   * `control`: the resolved `calibration_control` settings, including defaults.
#'
#'   Calibration and reference calculations preserve the caller's RNG exactly; only sampling the
#'   experiment itself (the returned profile rows) advances it.
#' @export
#' @md
#' @examples
#' design <- conjoint_design(c(2, 3))
#' set.seed(1)
#' data <- simulate_experiment(design, list(0.05, c(-0.05, 0.1)), units = 100, n_tasks = 3)
#' head(data)
#' attr(data, "dgp")$truth
#'
#' # heterogeneous preferences: individual AMCEs have SD 0.05 around the requested values
#' set.seed(1)
#' mixed <- simulate_experiment(design, list(0.05, c(-0.05, 0.1)), units = 100, n_tasks = 3,
#'                              sigma = 0.05)
#' attr(mixed, "dgp")$truth[, c("attribute", "level", "true_amce", "amce_sd")]
#'
#' # reuse a calibrated model for another experiment
#' second <- simulate_experiment(design, units = 100, n_tasks = 3, model = attr(data, "dgp"))
simulate_experiment <- function(design, true_amce = NULL, units, n_tasks, groups = NULL, sigma = 0,
                                dgp = "logit", latent_sigma = NULL,
                                model = NULL, calibration_control = list()) {
  if (!inherits(design, "cj_design")) {
    stop("`design` must be created with conjoint_design().", call. = FALSE)
  }
  if (!is.null(model)) {
    if (!inherits(model, "cj_dgp") || !identical(model$design, design)) {
      stop("`model` must be a prepared DGP for this exact design.", call. = FALSE)
    }
    if (!identical(model$dgp, "logit")) {
      stop("`model` must be a prepared logit DGP.", call. = FALSE)
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
    model <- prepare_dgp(design, true_amce, groups, sigma, dgp = dgp,
                         latent_sigma = latent_sigma, control = calibration_control)
  }

  group_of <- rep(seq_along(units), times = units)  # group index of each respondent
  data <- sample_tasks(design, n_tasks[group_of])
  if (!is.null(groups)) {
    data$group <- groups[group_of[data$respondent]]
  }

  # Keep the draw order stable: coefficient deviations, reference utilities, then choices.
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
  p1 <- stats::plogis(score[first] - score[!first])
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

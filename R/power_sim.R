#' Simulate power and estimation performance for a conjoint design
#'
#' Calibrates the choice model once, simulates independent experiments, and evaluates AMCEs and
#' differences between groups using respondent-clustered inference.
#'
#' @inheritParams conjoint_design
#' @inheritParams simulate_experiment
#' @inheritParams estimate_amce
#' @param groups NULL, or group names in the desired order. Report each group's AMCEs and differences
#'   between each subsequent group and the first group. Named sample sizes and effects are matched
#'   by name, retaining this order. A single group has AMCEs without differences.
#' @param sim_runs Positive number of experiments. Defaults to 1000.
#' @param seed Integer seed for the experiment streams. Required for reproducible runs.
#' @param cores Positive number of workers, default 1. Values above 1 use base R PSOCK workers;
#'   at most `sim_runs` workers are started. No global future plan or options are changed.
#' @param keep_runs Retain the estimates, inference and failure reasons from every run? Default FALSE.
#' @param n_levels,group_name,true_coef Deprecated named aliases for `levels`, `groups`, and
#'   `true_amce`. The default DGP is now calibrated logit; select `dgp = "odds"` explicitly to
#'   interpret `true_coef` as legacy score coefficients.
#' @param sigma.u_k Deprecated alias. In versions up to 0.2.1 it was the SD of respondent-level
#'   effects on the probability scale, so with the calibrated models it is used as `sigma`. With
#'   `dgp = "odds"` it is used as `latent_sigma`, which reproduces the old behaviour exactly.
#' @param n_attributes Deprecated attribute count, checked against `levels` when supplied.
#'
#' @details Each experiment has its own L'Ecuyer-CMRG stream, so results are identical across worker
#'   counts and the first runs remain unchanged when `sim_runs` increases. Calibration has its own
#'   seed argument in `calibration_control`. Equal seed values can overlap calibration and experiment
#'   draws; the final reference uses a fresh substream. The caller's random-number state is restored after the call.
#'   If the caller uses Box-Muller normals, a temporary PSOCK process initializes each private seed
#'   to preserve that generator's cached normal draw, including when `cores = 1`.
#'
#'   Bias and coverage use `true_amce`, the numerical reference for the generated population AMCE,
#'   with its uncertainty retained. Calibrated effects match `requested_amce` within `tolerance`,
#'   rather than necessarily equalling it. Null classification uses `requested_amce == 0`, or
#'   `true_amce == 0` when the requested AMCE is unavailable (the deprecated odds model uses scores).
#'   `null_status` distinguishes `exact`, `calibrated` and `non-null` targets. A single-group requested
#'   zero is exact in the calibrated models. Differences are exact for shared populations, the linear
#'   model or levels that are zero in both groups. Separately calibrated groups with equal nonzero
#'   requests generally produce a small residual contrast: their Type I error is approximate.
#'   `target_error_bound = abs(true_amce - requested_amce) + reference_half_width` is a nominal 99%
#'   simultaneous reference confidence bound, not a deterministic guarantee; it is NA for odds inputs.
#'   Type I error is printed with its Monte Carlo standard error only for null targets, not
#'   for nonsignificant estimates of nonzero effects. The column is omitted when no Type I error
#'   estimates are available. To check an effect's false-positive rate, rerun with that effect set
#'   to zero and the remaining design settings unchanged.
#'
#'   An independent final reference, with a fixed budget chosen after calibration acceptance,
#'   avoids selecting the reference estimate by the acceptance rule. Its MCSE is propagated into
#'   `bias_mcse`; coverage MCSE remains conditional on the reference, with sensitivity bounds supplied
#'   by [summarise_runs()]. Increasing `sim_runs` does not reduce reference uncertainty.
#'   Optional `calibration_control = list(reference_margin = 0.5)` reserves calibration headroom and
#'   plans a capped final reference budget; see [simulate_experiment()] for rules and diagnostics.
#'   This experimental option reduces inconclusive checks without guaranteeing confirmation.
#'   For calibrated nulls, `null_size_sensitivity` is the excess rejection probability in an unbiased,
#'   known-SE normal test at the target error bound, using `emp_se` as the SE. A warning appears when
#'   this exceeds `0.1 * sqrt(alpha * (1 - alpha) / n_valid)`. This threshold is a diagnostic choice;
#'   it is not a bound on size distortion for clustered, biased or non-normal estimates. Tighten
#'   `calibration_control` if the approximation is material. Exact nulls have sensitivity zero;
#'   non-null targets and unavailable reference bounds or empirical SEs have NA.
#'
#'   All measures condition on successful inference for that effect, using the effect's degrees of
#'   freedom in each run. Failed runs are counted, never treated as nonsignificant. Sampling or fit
#'   errors retain all effects with reason `run_error` and an error message. Invalid specifications,
#'   unavailable CR2 dependencies, calibration failures and worker startup errors stop the call.
#'
#'   Backward compatibility covers named arguments; migrate old positional calls to named arguments.
#'   The return value is now a list with a numeric performance table rather than formatted strings.
#'
#' @return A `cj_power` list with:
#'   * `performance`: one row per effect, including effect identifiers, requested and true AMCEs,
#'     covariance/inference methods, and all [summarise_runs()] measures and counts.
#'   * `truth`: effect identifiers and the model's input, requested AMCE, true AMCE, implied AMCE SD
#'     and reference precision, `null_status` and `target_error_bound`. `effect_id` links all result tables.
#'   * `parameters`, `diagnostics`: calibrated latent parameters and per-group calibration diagnostics.
#'   * `settings`: resolved design, sample sizes, DGP, inference, seeds, worker count and controls.
#'   * `failures`: counts by effect, failure reason and error message; empty when no inference fails.
#'   * `runs`: per-run estimates with `run` and `effect_id`, or NULL when `keep_runs = FALSE`.
#'   * `model`: the prepared DGP, reusable with [simulate_experiment()].
#' @seealso [conjoint_design()], [simulate_experiment()], [estimate_amce()], [summarise_runs()]
#' @export
#' @md
#' @examples
#' # 20 runs keep the example fast; use the default 1000 (or more) for a real power analysis
#' # Include a zero AMCE to report Type I error alongside power
#' result <- power_sim(levels = c(2, 3), true_amce = list(0.05, c(-0.05, 0)),
#'                     units = 100, n_tasks = 3, sim_runs = 20, seed = 42)
#' result
#' result$performance[, c("attribute", "level", "true_amce", "power", "power_mcse",
#'                        "type_1_error", "type_1_error_mcse", "n_valid")]
#'
#' by_group <- power_sim(levels = c(2, 2),
#'   true_amce = list(Z = list(0.05, 0), A = list(0.1, 0)), groups = c("Z", "A"),
#'   units = c(Z = 100, A = 80), n_tasks = 3, sim_runs = 20, seed = 42, keep_runs = TRUE)
#' subset(by_group$performance, type == "difference")
power_sim <- function(levels = NULL, true_amce = NULL, units, n_tasks, groups = NULL, sigma = 0,
                      dgp = c("logit", "linear", "odds"),
                      alpha = 0.05, vcov = c("CR1", "CR2"), sim_runs = 1000, seed, cores = 1,
                      inference = NULL, latent_sigma = NULL, calibration_control = list(),
                      keep_runs = FALSE, n_levels = NULL, group_name = NULL, true_coef = NULL,
                      sigma.u_k = NULL, n_attributes = NULL) {
  if (!missing(n_levels)) {
    deprecated_power_arg("n_levels", "levels", !missing(levels))
    levels <- n_levels
  }
  if (!missing(group_name)) {
    deprecated_power_arg("group_name", "groups", !missing(groups))
    groups <- group_name
  }
  if (!missing(true_coef)) {
    deprecated_power_arg("true_coef", "true_amce", !missing(true_amce),
      "The default DGP is calibrated logit; use dgp = \"odds\" for legacy scores.")
    true_amce <- true_coef
  }
  dgp <- match.arg(dgp)
  if (!missing(sigma.u_k)) {
    conflict <- !missing(sigma) || !missing(latent_sigma)
    if (dgp == "odds") {
      deprecated_power_arg("sigma.u_k", "latent_sigma", conflict,
        "With dgp = \"odds\" it is the score-scale SD of versions up to 0.2.1.")
      latent_sigma <- sigma.u_k
    } else {
      # In the old model, score coefficients were probability effects, so the old sigma.u_k was the SD
      # of respondent-level AMCEs: the closest current argument is the AMCE-scale sigma.
      deprecated_power_arg("sigma.u_k", "sigma", conflict,
        "It is used as the SD of respondent-level AMCEs on the probability scale, as in versions up to 0.2.1.")
      sigma <- sigma.u_k
    }
  }
  design <- conjoint_design(levels)
  if (!missing(n_attributes)) {
    deprecated_power_arg("n_attributes", "levels")
    if (positive_count(n_attributes, "n_attributes") != length(design$levels)) {
      stop("`n_attributes` must match the number of attributes in `levels`.", call. = FALSE)
    }
  }
  groups <- check_groups(groups)
  units <- per_group(units, groups, "units")
  n_tasks <- per_group(n_tasks, groups, "n_tasks")
  sim_runs <- positive_count(sim_runs, "sim_runs")
  cores <- positive_count(cores, "cores")
  if (missing(seed) || !is.numeric(seed) || length(seed) != 1 || !is.finite(seed) ||
      seed != round(seed) || abs(seed) > .Machine$integer.max) {
    stop("`seed` must be a single supported integer.", call. = FALSE)
  }
  seed <- as.integer(seed)
  if (!is.logical(keep_runs) || length(keep_runs) != 1 || is.na(keep_runs)) {
    stop("`keep_runs` must be TRUE or FALSE.", call. = FALSE)
  }
  vcov <- match.arg(vcov)
  inference <- check_inference(vcov, inference, alpha)
  model <- prepare_dgp(design, true_amce, groups, sigma, dgp, latent_sigma, calibration_control)
  truth <- data.frame(effect_id = seq_len(nrow(model$truth)), model$truth, row.names = NULL)
  worker <- power_worker(model, units, n_tasks, vcov, inference, alpha)
  workers <- min(cores, sim_runs)
  results <- with_reference_seed(seed, {
    streams <- vector("list", sim_runs)
    streams[[1]] <- get(".Random.seed", envir = .GlobalEnv)
    for (i in seq_len(sim_runs - 1L)) streams[[i + 1L]] <- parallel::nextRNGStream(streams[[i]])
    if (workers == 1L) {
      lapply(streams, worker)
    } else {
      cluster <- parallel::makePSOCKcluster(workers)
      on.exit(parallel::stopCluster(cluster), add = TRUE)
      parallel::clusterCall(cluster, function(paths) .libPaths(paths), .libPaths())
      parallel::parLapply(cluster, streams, worker)
    }
  })
  runs <- do.call(rbind, lapply(seq_along(results), function(i) {
    data.frame(run = i, results[[i]], row.names = NULL)
  }))
  performance <- do.call(rbind, lapply(truth$effect_id, function(id) {
    rows <- runs[runs$effect_id == id, ]
    summarise_runs(rows$estimate, rows$std.error, truth$true_amce[id], alpha, rows$df,
      null = if (is.na(truth$requested_amce[id])) truth$true_amce[id] == 0 else truth$requested_amce[id] == 0,
      reference_mcse = truth$reference_mcse[id], reference_half_width = truth$reference_half_width[id])
  }))
  performance <- data.frame(truth, vcov = vcov, inference = inference, performance, row.names = NULL)
  performance <- diagnose_nulls(performance, alpha)
  class(performance) <- c("cj_performance", "data.frame")
  failures <- power_failures(runs, truth)
  settings <- list(design = design, units = units, n_tasks = n_tasks, groups = groups,
                   sigma = sigma, latent_sigma = latent_sigma, dgp = dgp, alpha = alpha,
                   vcov = vcov, inference = inference, sim_runs = sim_runs, seed = seed,
                   cores = cores, workers = workers, keep_runs = keep_runs,
                   calibration_control = model$control, rng = c("L'Ecuyer-CMRG", "Inversion", "Rejection"))
  if (!is.null(groups)) {
    names(settings$units) <- names(settings$n_tasks) <- groups
  }
  structure(list(performance = performance, truth = truth, parameters = model$parameters,
                 diagnostics = model$diagnostics, settings = settings, failures = failures,
                 runs = if (keep_runs) runs else NULL, model = model), class = "cj_power")
}

# Known-SE, unbiased normal benchmark only: this is not a size bound for the fitted cluster test.
diagnose_nulls <- function(performance, alpha) {
  b <- performance$target_error_bound
  calibrated <- performance$null_status == "calibrated"
  eligible <- calibrated & is.finite(b) & is.finite(performance$emp_se) & performance$emp_se > 0
  sensitivity <- rep(NA_real_, nrow(performance))
  sensitivity[performance$null_status == "exact"] <- 0
  shift <- b[eligible] / performance$emp_se[eligible]
  critical <- stats::qnorm(1 - alpha / 2)
  sensitivity[eligible] <- pmax(0, stats::pnorm(-critical - shift) +
    stats::pnorm(shift - critical) - alpha)
  performance$null_size_sensitivity <- sensitivity
  # One tenth of the nominal null MCSE is a diagnostic threshold, not a statistical guarantee.
  threshold <- 0.1 * sqrt(alpha * (1 - alpha) / pmax(1, performance$n_valid))
  material <- eligible & !is.na(sensitivity) & sensitivity > threshold
  if (any(material)) warning("Calibrated null sensitivity exceeds one tenth of the nominal Type I error MCSE for effect(s) ",
    paste(which(material), collapse = ", "), ". Type I error is approximate; tighten calibration_control. ",
    "This normal-theory diagnostic is not a guarantee for clustered inference.", call. = FALSE)
  performance
}

deprecated_power_arg <- function(old, new, conflict = FALSE, note = "") {
  if (conflict) stop("supply only one of `", old, "` and its replacement `", new, "`",
                     if (old == "sigma.u_k") " (and no other heterogeneity argument)", ".", call. = FALSE)
  warning("`", old, "` is deprecated; use `", new, "` instead.",
          if (nzchar(note)) paste0(" ", note), call. = FALSE)
}

positive_count <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 1 ||
      x != round(x) || x > .Machine$integer.max) {
    stop("`", arg, "` must be a positive supported whole number.", call. = FALSE)
  }
  as.integer(x)
}

# Exact tuple matching: display labels and delimiter-containing user names are never parsed.
match_effect_rows <- function(template, data) {
  columns <- c("type", "group_name", "reference_group", "attribute", "level", "reference_level")
  codes <- lapply(columns, function(nm) match(data[[nm]], unique(template[[nm]])))
  if (anyNA(unlist(codes))) stop("estimated effects do not match the design.", call. = FALSE)
  keys <- do.call(paste, c(codes, sep = ":"))
  expected <- do.call(paste, c(lapply(columns, function(nm) {
    match(template[[nm]], unique(template[[nm]]))
  }), sep = ":"))
  index <- match(expected, keys)
  if (anyNA(index) || anyDuplicated(keys) || length(keys) != length(expected)) {
    stop("estimated effects do not match the design.", call. = FALSE)
  }
  index
}

# Send the exact currently loaded implementation, not a possibly stale installed package, to PSOCK
# workers. This bounded environment contains only sampling/estimation functions and the prepared model.
power_worker <- function(model, units, n_tasks, vcov, inference, alpha) {
  env <- new.env(parent = baseenv())
  functions <- c("simulate_experiment", "sample_tasks", "draw_profiles", "profile_scores", "odds_choice",
                 "check_groups", "per_group", "estimate_amce", "amce_model_data", "check_inference",
                 "match_effect_rows")
  for (nm in functions) {
    fn <- get(nm, envir = environment(power_worker))
    environment(fn) <- env
    env[[nm]] <- fn
  }
  list2env(list(model = model, units = units, n_tasks = n_tasks, vcov = vcov,
                inference = inference, alpha = alpha), envir = env)
  eval(quote(function(stream) {
    assign(".Random.seed", stream, envir = .GlobalEnv)
    template <- model$truth[c("type", "group", "group_name", "reference_group", "attribute",
                              "level", "reference_level")]
    out <- tryCatch({
      data <- simulate_experiment(model$design, units = units, n_tasks = n_tasks, model = model)
      fit <- estimate_amce(data, model$design, groups = model$groups, vcov = vcov,
                           inference = inference, alpha = alpha)
      fit <- fit[match_effect_rows(template, fit), , drop = FALSE]
      # Remove the per-fit covariance matrix; it is not needed for Monte Carlo summaries.
      attr(fit, "vcov") <- NULL
      message <- attr(fit, "covariance_error")
      attr(fit, "covariance_error") <- NULL
      fit$error_message <- NA_character_
      if (!is.null(message)) fit$error_message[fit$failure_reason %in% "covariance_failure"] <- message
      fit
    }, error = function(e) {
      out <- template
      for (nm in c("n_clusters_effect", "estimate", "std.error", "df", "statistic", "p.value",
                   "conf.low", "conf.high", "n_clusters", "rank")) out[[nm]] <- NA_real_
      out$vcov <- vcov
      out$inference <- inference
      out$failure_reason <- "run_error"
      out$status <- "failed"
      out$error_message <- conditionMessage(e)
      out
    })
    out$effect_id <- seq_len(nrow(template))
    out
  }), envir = env)
}

power_failures <- function(runs, truth) {
  failed <- runs[runs$status == "failed", c("effect_id", "failure_reason", "error_message"), drop = FALSE]
  counts <- unique(failed)
  counts$n_failed <- vapply(seq_len(nrow(counts)), function(i) {
    sum(failed$effect_id == counts$effect_id[i] & failed$failure_reason == counts$failure_reason[i] &
          if (is.na(counts$error_message[i])) is.na(failed$error_message)
          else !is.na(failed$error_message) & failed$error_message == counts$error_message[i])
  }, integer(1))
  labels <- truth[match(counts$effect_id, truth$effect_id),
                  c("effect_id", "type", "group", "group_name", "reference_group", "attribute", "level", "reference_level"),
                  drop = FALSE]
  data.frame(labels, counts[c("failure_reason", "error_message", "n_failed")], row.names = NULL)
}

#' @export
print.cj_power <- function(x, digits = 3, ...) {
  cat("Conjoint simulation:", x$settings$sim_runs, "runs;", x$settings$dgp, "DGP;",
      x$settings$vcov, "/", x$settings$inference, "inference.\n")
  print(x$performance, digits = digits, ...)
  if (nrow(x$failures)) cat("See $failures for counts and reasons by effect.\n")
  invisible(x)
}

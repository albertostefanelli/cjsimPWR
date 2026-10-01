# Reference calculations and calibration for paired, uniformly randomized profiles.
# Restore the caller's RNG; equal calibration/experiment seeds can still share initial draws.
with_reference_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  old_kind <- RNGkind()
  preserve_normal_cache <- old_kind[2] == "Box-Muller"
  on.exit({
    # Calling RNGkind() with arguments destroys Box-Muller's cached second deviate.
    # Restoring the seed alone restores all three RNG kinds on the next RNG access.
    if (!preserve_normal_cache || !had_seed) do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  })
  if (preserve_normal_cache) {
    assign(".Random.seed", private_rng_seed(seed), envir = .GlobalEnv)
  } else {
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(seed)
  }
  force(expr)
}

# Only needed for callers using Box-Muller: even set.seed() under a different normal generator
# clears its hidden cache. Seed in one short-lived process, then install the stream by assignment.
private_rng_seed <- function(seed) {
  cluster <- parallel::makePSOCKcluster(1L)
  on.exit(parallel::stopCluster(cluster))
  parallel::clusterCall(cluster, function(seed) {
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(seed)
    get(".Random.seed", envir = .GlobalEnv)
  }, seed)[[1]]
}

dgp_controls <- function(control = list()) {
  defaults <- list(seed = 104729L, maxit = 100L, solver_tol = 1e-6, tolerance = 0.001,
                   exact_max_pairs = 100000000L, exact_max_rows = 20000L,
                   calibration_pairs = 16384L, calibration_draws = 2048L,
                   verification_pairs = 32768L, verification_draws = 1024L,
                   verification_batches = 16L, max_verification_batches = 128L, max_attempts = 4L,
                   reference_margin = 0, max_reference_batches = 512L)
  if (!is.list(control) || (length(control) && (is.null(names(control)) || anyDuplicated(names(control)) ||
      any(!names(control) %in% names(defaults))))) stop("unknown or unnamed calibration control.", call. = FALSE)
  defaults[names(control)] <- control
  margin <- defaults$reference_margin
  if (!is.numeric(margin) || length(margin) != 1 || !is.finite(margin) || margin < 0 || margin >= 1) {
    stop("calibration control 'reference_margin' must be a number in [0, 1).", call. = FALSE)
  }
  for (nm in setdiff(names(defaults), "reference_margin")) {
    v <- defaults[[nm]]
    if (!is.numeric(v) || length(v) != 1 || !is.finite(v) || v <= 0) {
      stop("calibration control '", nm, "' must be one positive finite number.", call. = FALSE)
    }
    if (!nm %in% c("solver_tol", "tolerance") && (v != round(v) || v > .Machine$integer.max / 4)) {
      stop("calibration control '", nm, "' must be a supported whole number.", call. = FALSE)
    }
  }
  if (defaults$verification_batches < 4 || defaults$verification_draws < 2 || defaults$calibration_draws < 2 ||
      defaults$max_verification_batches < defaults$verification_batches || defaults$maxit > 100) {
    stop("calibration needs at least 4 verification batches, 2 draws, and at most 100 solver iterations.", call. = FALSE)
  }
  if (margin > 0 && defaults$max_reference_batches < defaults$verification_batches) {
    stop("max_reference_batches must be at least verification_batches when reference_margin > 0.", call. = FALSE)
  }
  defaults
}

effect_metadata <- function(design) {
  do.call(rbind, lapply(seq_along(design$levels), function(k) {
    lv <- design$levels[[k]]
    data.frame(attribute = names(design$levels)[k], level = lv[-1], reference_level = lv[1],
               attribute_index = k, stringsAsFactors = FALSE)
  }))
}

# Independent profile dummy columns, in design order. Used only by bounded reference calculations.
reference_dummies <- function(codes, n_levels) {
  do.call(cbind, lapply(seq_along(n_levels), function(k) {
    vapply(2:n_levels[k], function(l) as.numeric(codes[, k] == l), numeric(nrow(codes)))
  }))
}

# Analytical sizes, before any profile/difference allocation. Count in doubles to avoid integer
# overflow; non-finite sizes fail eligibility. The workspace target allows four reference-sized
# matrices (including construction temporaries) and four rows-by-128 probability work matrices,
# not the whole R process's memory.
exact_profile_size <- function(n_levels) {
  levels <- as.double(n_levels)
  rows <- (prod(levels * (levels - 1) + 1) - 1) / 2
  columns <- sum(levels - 1)
  matrix_bytes <- 8 * rows * columns
  probability_bytes <- 8 * rows * 128
  list(pairs = prod(levels)^2, rows = rows, columns = columns,
       largest_allocation = max(matrix_bytes, probability_bytes),
       workspace_bytes = 4 * matrix_bytes + 4 * probability_bytes)
}

exact_profile_fits <- function(size) {
  is.finite(size$workspace_bytes) && size$workspace_bytes <= 256 * 1024^2 &&
    size$largest_allocation / 8 <= .Machine$integer.max
}

# Shared by reference construction and verification, including the homogeneous shortcut.
exact_profile_eligible <- function(n_levels, exact_max_pairs, exact_max_rows) {
  size <- exact_profile_size(n_levels)
  is.finite(size$pairs) && size$pairs <= exact_max_pairs && size$rows <= exact_max_rows &&
    exact_profile_fits(size)
}

# Store only positive nonzero differences; the builder indexes their negatives and the zero block.
# Positive representatives have their first nonzero dummy entry positive.
profile_difference_block <- function(levels) {
  positive <- matrix(0, levels * (levels - 1) / 2, levels - 1)
  row <- 0L
  for (a in 2:levels) {
    row <- row + 1L
    positive[row, a - 1L] <- 1
    if (a < levels) for (b in (a + 1L):levels) {
      row <- row + 1L
      positive[row, a - 1L] <- 1
      positive[row, b - 1L] <- -1
    }
  }
  positive
}

exact_profile_reference <- function(n_levels) {
  size <- exact_profile_size(n_levels)
  if (!exact_profile_fits(size)) stop("exact profile reference exceeds the 256 MiB workspace budget.", call. = FALSE)
  sizes <- n_levels * (n_levels - 1) + 1
  rows <- size$rows
  pairs <- size$pairs
  blocks <- lapply(n_levels, profile_difference_block)
  columns <- split(seq_len(sum(n_levels - 1)), rep(seq_along(n_levels), n_levels - 1))
  delta <- matrix(0, rows, sum(n_levels - 1))
  mass <- numeric(rows)
  offset <- 0L
  # Partition by the first nonzero attribute. Earlier attributes match, this block is positive,
  # and later blocks take every value. This directly generates one row per {d, -d}, never all pairs.
  for (k in seq_along(n_levels)) {
    later <- seq_along(n_levels)[seq_along(n_levels) > k]
    indices <- expand.grid(c(list(1L + seq_len((sizes[k] - 1) / 2)), lapply(sizes[later], seq_len)))
    selected <- offset + seq_len(nrow(indices))
    delta[selected, columns[[k]]] <- blocks[[k]][indices[[1]] - 1L, , drop = FALSE]
    multiplicity <- rep(prod(n_levels[seq_len(k - 1L)]), nrow(indices))
    for (i in seq_along(later)) {
      j <- later[i]
      index <- indices[[i + 1L]]
      positive_rows <- (sizes[j] - 1) / 2
      sign <- ifelse(index == 1L, 0, ifelse(index <= positive_rows + 1L, 1, -1))
      delta[selected, columns[[j]]] <- blocks[[j]][(index - 2L) %% positive_rows + 1L, , drop = FALSE] * sign
      multiplicity <- multiplicity * ifelse(index == 1L, n_levels[j], 1)
    }
    # The omitted all-zero row has zero contribution. Its mass must NOT be redistributed.
    mass[selected] <- 2 * multiplicity / pairs
    offset <- offset + nrow(indices)
  }
  weights <- delta
  for (k in seq_along(columns)) {
    j <- columns[[k]]
    weights[, j] <- (delta[, j, drop = FALSE] + rowSums(delta[, j, drop = FALSE])) * n_levels[k] / 2 * mass
  }
  list(delta = delta, weights = weights, exact = TRUE, pairs = as.integer(pairs))
}

# Under independent uniform randomization, AMCE = Cov(Z)^-1 E[(Z1-Z2)(P1-.5)]/2.
# Exact integration and the MC oracle use the same identity. MC uses a control variate with
# known expectation beta/4 to reduce profile-integration error.
profile_reference <- function(n_levels, pairs, exact_max_pairs, exact_max_rows) {
  if (exact_profile_eligible(n_levels, exact_max_pairs, exact_max_rows)) return(exact_profile_reference(n_levels))
  z1 <- reference_dummies(draw_profiles(n_levels, pairs), n_levels)
  z2 <- reference_dummies(draw_profiles(n_levels, pairs), n_levels)
  delta <- z1 - z2
  weights <- delta
  blocks <- split(seq_len(ncol(delta)), rep(seq_along(n_levels), n_levels - 1L))
  for (k in seq_along(blocks)) {
    j <- blocks[[k]]
    weights[, j] <- (delta[, j, drop = FALSE] + rowSums(delta[, j, drop = FALSE])) * n_levels[k] / 2
  }
  list(delta = delta, weights = weights / nrow(delta), exact = FALSE,
       pairs = nrow(delta))
}

# Conditional AMCEs for each respondent coefficient vector, integrating over profile randomization.
conditional_amces <- function(beta, reference) {
  beta <- as.matrix(beta)
  result <- matrix(0, nrow(beta), ncol(beta))
  blocks <- split(seq_len(nrow(beta)), ceiling(seq_len(nrow(beta)) / 128L))
  slope <- 0.25
  for (rows in blocks) {
    b <- beta[rows, , drop = FALSE]
    eta <- reference$delta %*% t(b)
    prob <- stats::plogis(eta)
    if (reference$exact) {
      result[rows, ] <- t(crossprod(reference$weights, prob - 0.5))
    } else {
      result[rows, ] <- slope * b + t(crossprod(reference$weights, prob - 0.5 - slope * eta))
    }
  }
  # A zero coefficient gives exactly the same utility as the reference level for that respondent.
  result[beta == 0] <- 0
  result
}

reference_sample <- function(n_levels, control, pairs, draws, heterogeneous, exact_reference = NULL) {
  ref <- if (is.null(exact_reference)) {
    profile_reference(n_levels, pairs, control$exact_max_pairs, control$exact_max_rows)
  } else exact_reference
  # Independent profile samples remove the squared-integration-error bias from the second moment.
  ref2 <- if (ref$exact || !heterogeneous) NULL else {
    profile_reference(n_levels, pairs, control$exact_max_pairs, control$exact_max_rows)
  }
  U <- if (heterogeneous) matrix(stats::rnorm(draws * (sum(n_levels - 1) + length(n_levels))), draws) else NULL
  list(ref = ref, ref2 = ref2, U = U, attribute = rep(seq_along(n_levels), n_levels - 1))
}

reference_moments <- function(gamma, latent_sd, sample, baseline_sd = NULL) {
  if (all(latent_sd == 0)) {
    value <- as.numeric(conditional_amces(matrix(gamma, 1), sample$ref))
    return(list(mean = value, second = value^2, sd = rep(0, length(value))))
  }
  U <- rbind(sample$U, -sample$U)
  q <- length(gamma)
  beta <- sweep(sweep(U[, seq_len(q), drop = FALSE], 2, latent_sd, "*"), 2, gamma, "+")
  if (!is.null(baseline_sd)) {
    beta <- beta - sweep(U[, q + sample$attribute, drop = FALSE], 2, baseline_sd[sample$attribute], "*")
  }
  a <- conditional_amces(beta, sample$ref)
  b <- if (is.null(sample$ref2)) a else conditional_amces(beta, sample$ref2)
  mu <- colMeans((a + b) / 2)
  second <- colMeans(a * b)
  list(mean = mu, second = second, sd = sqrt(pmax(0, second - mu^2)))
}

# Damped Broyden iteration, with finite-difference Jacobian refresh when a step cannot improve.
# A failed solve is numerical non-convergence, not a proof that the requested AMCEs are infeasible.
solve_amce_moments <- function(fn, x, tolerance, maxit) {
  value <- fn(x)
  jacobian <- function(x, fx) {
    vapply(seq_along(x), function(j) {
      h <- 1e-5 * max(1, abs(x[j]))
      xh <- x; xh[j] <- xh[j] + h
      (fn(xh) - fx) / h
    }, numeric(length(fx)))
  }
  if (max(abs(value)) <= tolerance) return(list(x = x, residual = value, iterations = 0L, converged = TRUE))
  J <- jacobian(x, value)
  for (iteration in seq_len(maxit)) {
    accepted <- FALSE
    for (refresh in 0:1) {
      if (refresh) J <- jacobian(x, value)
      step <- tryCatch(as.numeric(solve(J, -value)), error = function(e) rep(NA_real_, length(x)))
      if (any(!is.finite(step))) next
      if (max(abs(step)) > 5) step <- step * 5 / max(abs(step))
      for (scale in 2^-(0:14)) {
        candidate <- x + scale * step
        next_value <- fn(candidate)
        if (all(is.finite(next_value)) && sum(next_value^2) < sum(value^2)) {
          accepted <- TRUE
          break
        }
      }
      if (accepted) break
    }
    if (!accepted) break
    dx <- candidate - x
    J <- J + tcrossprod(next_value - value - as.numeric(J %*% dx), dx) / sum(dx^2)
    x <- candidate; value <- next_value
    if (max(abs(value)) <= tolerance) break
  }
  list(x = x, residual = value, iterations = iteration,
       converged = max(abs(value)) <= tolerance)
}

# Independent batches include both profile integration error and coefficient-draw uncertainty.
# Verification bounds account for every effect, SD, calibration attempt and verification look.
# A fixed-budget final reference has just one look and is never used to select the model.
verify_dgp <- function(gamma, latent_sd, n_levels, control, target, sigma = 0,
                       structural_zero = rep(FALSE, length(gamma)), baseline_sd = NULL,
                       fixed_batches = NULL, acceptance_margin = 0, exact_reference = NULL) {
  heterogeneous <- any(latent_sd > 0)
  exact <- exact_profile_eligible(n_levels, control$exact_max_pairs, control$exact_max_rows)
  if (exact && is.null(exact_reference)) {
    exact_reference <- profile_reference(n_levels, control$verification_pairs, control$exact_max_pairs,
                                         control$exact_max_rows)
  }
  if (exact && !heterogeneous) {
    sample <- reference_sample(n_levels, control, control$verification_pairs, 2, FALSE, exact_reference)
    moments <- reference_moments(gamma, latent_sd, sample)
    return(c(moments, list(mcse = rep(0, length(gamma)), sd_mcse = rep(0, length(gamma)),
                          half_width = rep(0, length(gamma)), sd_half_width = rep(0, length(gamma)),
                          accepted = max(abs(moments$mean - target)) <=
                            (1 - acceptance_margin) * control$tolerance,
                          batches = 0L, draws = 0L, pairs = sample$ref$pairs, method = "exact")))
  }
  n <- length(gamma)
  batches <- if (is.null(fixed_batches)) control$max_verification_batches else fixed_batches
  means <- seconds <- matrix(NA_real_, batches, n)
  looks <- if (!is.null(fixed_batches)) fixed_batches else unique(pmin(control$max_verification_batches,
                       control$verification_batches * 2^(0:ceiling(log2(control$max_verification_batches / control$verification_batches)))))
  family <- if (is.null(control$comparisons_multiplier)) 1 else control$comparisons_multiplier
  attempts <- if (is.null(fixed_batches)) control$max_attempts else 1L
  comparisons <- n * (if (heterogeneous) 2 else 1) * length(looks) * attempts * family
  for (batch in seq_len(batches)) {
    sample <- reference_sample(n_levels, control, control$verification_pairs,
                               control$verification_draws, heterogeneous, exact_reference)
    moment <- reference_moments(gamma, latent_sd, sample, baseline_sd)
    means[batch, ] <- moment$mean; seconds[batch, ] <- moment$second
    if (!batch %in% looks) next
    mu <- colMeans(means[seq_len(batch), , drop = FALSE])
    mcse <- apply(means[seq_len(batch), , drop = FALSE], 2, stats::sd) / sqrt(batch)
    mu[structural_zero] <- 0; mcse[structural_zero] <- 0
    second <- colMeans(seconds[seq_len(batch), , drop = FALSE])
    sd <- if (heterogeneous) sqrt(pmax(0, second - mu^2)) else rep(0, n)
    sd_mcse <- rep(0, n)
    if (heterogeneous) {
      for (j in seq_len(n)) sd_mcse[j] <- if (sd[j] > 0) {
        stats::sd(seconds[seq_len(batch), j] - 2 * mu[j] * means[seq_len(batch), j]) / (2 * sd[j] * sqrt(batch))
      } else Inf
    }
    critical <- stats::qt(1 - 0.01 / (2 * comparisons), batch - 1)
    width <- critical * mcse; sd_width <- critical * sd_mcse
    accepted <- all(abs(mu - target) + width <= (1 - acceptance_margin) * control$tolerance)
    if (sigma > 0) {
      accepted <- accepted && all(abs(sd - sigma) + sd_width <=
                                   (1 - acceptance_margin) * min(0.005, 0.05 * sigma))
    }
    if (accepted) break
  }
  list(mean = mu, second = second, sd = sd, mcse = mcse, sd_mcse = sd_mcse,
       half_width = width, sd_half_width = sd_width, accepted = accepted, batches = batch,
       draws = if (heterogeneous) 2 * control$verification_draws * batch else 0L,
       pairs = sample$ref$pairs * batch, method = if (exact) "exact profiles / Monte Carlo coefficients" else "Monte Carlo")
}

# Failure to confirm containment is inconclusive unless an interval is disjoint from the band.
# SDs are tested only when `sigma > 0`: a fixed `latent_sigma` is not an AMCE-SD target.
reference_contradicted <- function(reference, target, sigma, tolerance) {
  amce <- any(abs(reference$mean - target) - reference$half_width > tolerance)
  sd <- sigma > 0 &&
    any(abs(reference$sd - sigma) - reference$sd_half_width > min(0.005, 0.05 * sigma))
  amce || sd
}

# Plan before any final draw. Scale independent batches, keeping their integration settings fixed.
# Doubling pilot variances is a conservative planning allowance, not a confidence bound: verification
# selected this pilot, and batch-t / delta-method SD intervals are themselves approximations.
plan_reference <- function(verified, control, sigma, heterogeneous) {
  n <- length(verified$mean)
  targeted_sd <- sigma > 0
  quantities <- data.frame(quantity = c(rep("amce", n), if (targeted_sd) rep("sd", n)),
    effect_index = c(seq_len(n), if (targeted_sd) seq_len(n)),
    tolerance = c(rep(control$tolerance, n), if (targeted_sd) rep(min(0.005, 0.05 * sigma), n)))
  quantities$target_half_width <- control$reference_margin * quantities$tolerance / 2
  mcse <- c(verified$mcse, if (targeted_sd) verified$sd_mcse)
  variance_inflation <- 2
  family <- if (is.null(control$comparisons_multiplier)) 1 else control$comparisons_multiplier
  comparisons <- n * (if (heterogeneous) 2 else 1) * family
  if (verified$batches == 0L) {
    batches <- 0L
    predicted <- rep(0, nrow(quantities))
    cap_limited <- FALSE
  } else {
    batch_sd <- mcse * sqrt(verified$batches * variance_inflation)
    predict_width <- function(b) stats::qt(1 - 0.01 / (2 * comparisons), b - 1) * batch_sd / sqrt(b)
    meets_target <- function(b) all(predict_width(b) <= quantities$target_half_width)
    lower <- control$verification_batches
    upper <- control$max_reference_batches
    cap_limited <- !meets_target(upper)
    if (cap_limited) {
      batches <- upper
    } else {
      # Smallest allowed batch count meeting the predicted width for every targeted quantity.
      while (lower < upper) {
        mid <- floor((lower + upper) / 2)
        if (meets_target(mid)) upper <- mid else lower <- mid + 1
      }
      batches <- lower
    }
    predicted <- predict_width(batches)
  }
  quantities$predicted_half_width <- predicted
  list(margin = control$reference_margin, variance_inflation = variance_inflation,
       batches = as.integer(batches), max_batches = control$max_reference_batches,
       cap_limited = cap_limited, predicted_precision_met = !cap_limited, quantities = quantities)
}

record_reference_precision <- function(plan, reference) {
  q <- plan$quantities
  q$half_width <- ifelse(q$quantity == "amce", reference$half_width[q$effect_index],
                         reference$sd_half_width[q$effect_index])
  q$precision_met <- q$half_width <= q$target_half_width
  plan$quantities <- q
  plan$precision_met <- all(q$precision_met)
  plan
}

prepare_dgp <- function(design, true_amce, groups = NULL, sigma = 0, dgp = "logit",
                        latent_sigma = NULL, control = list()) {
  if (!inherits(design, "cj_design")) stop("`design` must be created with conjoint_design().", call. = FALSE)
  groups <- check_groups(groups)
  target <- amce_matrix(true_amce, design, groups)
  if (!is.character(dgp) || length(dgp) != 1L || is.na(dgp) || dgp != "logit") {
    stop("`dgp` must be \"logit\".", call. = FALSE)
  }
  dgp <- "logit"
  scalar_sd <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0
  if (!scalar_sd(sigma)) stop("`sigma` must be a single non-negative number.", call. = FALSE)
  if (!is.null(latent_sigma) && !scalar_sd(latent_sigma)) stop("`latent_sigma` must be a single non-negative number.", call. = FALSE)
  if (!is.null(latent_sigma) && sigma != 0) stop("supply AMCE-scale `sigma` or `latent_sigma`, not both.", call. = FALSE)
  control <- dgp_controls(control)
  meta <- effect_metadata(design)
  p <- nrow(meta)
  n_groups <- nrow(target)
  gamma <- latent_sd <- raw_sd <- matrix(0, n_groups, p)
  baseline_sd <- matrix(0, n_groups, length(design$n_levels))
  results <- vector("list", n_groups)
  parameter_tables <- vector("list", n_groups)
  diagnostics <- vector("list", n_groups)
  group_control <- control
  group_control$comparisons_multiplier <- n_groups
  if (n_groups > 1) group_control$tolerance <- control$tolerance / 2
  for (g in seq_len(n_groups)) {
    requested <- target[g, ]
    previous <- which(vapply(seq_len(g - 1L), function(i) identical(target[i, ], requested), logical(1)))
    if (length(previous)) {
      # Identical inputs imply identical populations, including an exactly zero subgroup difference.
      i <- previous[1]
      gamma[g, ] <- gamma[i, ]; latent_sd[g, ] <- latent_sd[i, ]
      raw_sd[g, ] <- raw_sd[i, ]; baseline_sd[g, ] <- baseline_sd[i, ]
      diagnostics[[g]] <- diagnostics[[i]]
      reference <- diagnostics[[g]]$reference
    } else {
      fitted <- with_reference_seed(control$seed + g - 1L,
        calibrate_dgp_group(design, requested, sigma, latent_sigma, group_control))
      gamma[g, ] <- fitted$gamma
      latent_sd[g, ] <- fitted$latent_sd
      raw_sd[g, ] <- fitted$raw_sd
      baseline_sd[g, ] <- fitted$baseline_sd
      reference <- fitted$diagnostics$reference
      diagnostics[[g]] <- fitted$diagnostics
      diagnostics[[g]]$seed <- control$seed + g - 1L
    }
    if (is.null(diagnostics[[g]]$verification)) diagnostics[[g]]$verification <- reference
    if (control$reference_margin > 0) {
      if (is.null(diagnostics[[g]]$reference_plan)) {
        # All-zero models need neither a pilot nor fresh simulation draws.
        diagnostics[[g]]$reference_plan <- record_reference_precision(
          plan_reference(reference, group_control, sigma, any(raw_sd[g, ] > 0)), reference)
      }
      q <- diagnostics[[g]]$reference_plan$quantities
      q$attribute <- meta$attribute[q$effect_index]
      q$level <- meta$level[q$effect_index]
      diagnostics[[g]]$reference_plan$quantities <- q
    }
    results[[g]] <- data.frame(type = "amce", group = if (is.null(groups)) NA_character_ else groups[g],
      group_name = if (is.null(groups)) NA_character_ else groups[g], reference_group = NA_character_,
      meta[c("attribute", "level", "reference_level")], input = requested,
      requested_amce = requested,
      true_amce = reference$mean,
      amce_sd = reference$sd, reference_mcse = reference$mcse, reference_half_width = reference$half_width,
      sd_mcse = reference$sd_mcse, sd_half_width = reference$sd_half_width, stringsAsFactors = FALSE)
    parameter_tables[[g]] <- data.frame(group = if (is.null(groups)) NA_character_ else groups[g],
      meta[c("attribute", "level", "reference_level")], latent_mean = gamma[g, ],
      latent_sd = latent_sd[g, ], utility_sd = raw_sd[g, ],
      reference_utility_sd = baseline_sd[g, meta$attribute_index], stringsAsFactors = FALSE)
  }
  truth <- do.call(rbind, results)
  if (n_groups > 1) {
    differences <- lapply(2:n_groups, function(g) {
      a <- results[[g]]; b <- results[[1]]
      a$type <- "difference"; a$group <- paste(groups[g], "-", groups[1]); a$reference_group <- groups[1]
      for (nm in c("input", "requested_amce", "true_amce")) a[[nm]] <- a[[nm]] - b[[nm]]
      a$reference_mcse <- sqrt(a$reference_mcse^2 + b$reference_mcse^2)
      a$reference_half_width <- a$reference_half_width + b$reference_half_width
      shared <- identical(gamma[g, ], gamma[1, ]) && identical(raw_sd[g, ], raw_sd[1, ]) &&
        identical(baseline_sd[g, ], baseline_sd[1, ])
      if (shared) {
        # The reference is shared, so its error cancels instead of adding independent variances.
        a$true_amce <- 0
        a$reference_mcse <- a$reference_half_width <- 0
      }
      a$amce_sd <- a$sd_mcse <- a$sd_half_width <- NA_real_  # no paired respondent-level contrast across populations
      a
    })
    truth <- rbind(truth, do.call(rbind, differences))
  }
  # A simultaneous Monte Carlo confidence bound on distance from the requested target, not a
  # deterministic bound.
  truth$target_error_bound <- abs(truth$true_amce - truth$requested_amce) + truth$reference_half_width
  rownames(truth) <- NULL
  parameters <- do.call(rbind, parameter_tables)
  rownames(parameters) <- NULL
  structure(list(design = design, groups = groups, dgp = dgp, input = target, sigma = sigma,
                 latent_sigma = latent_sigma, gamma = gamma, latent_sd = latent_sd,
                 raw_sd = raw_sd, baseline_sd = baseline_sd,
                 truth = truth, parameters = parameters, diagnostics = diagnostics, control = control), class = "cj_dgp")
}

# Internal warning eligibility, in truth-table order; never inferred from a rounded residual.
approximate_nulls <- function(model) {
  input <- model$input
  # A requested zero uses exchangeable level and reference utilities, including their joint
  # Gaussian deviations. This preserves the population null under heterogeneity.
  flags <- rep(FALSE, length(input))
  for (g in seq_len(nrow(input))[-1]) {
    shared <- identical(model$gamma[g, ], model$gamma[1, ]) &&
      identical(model$raw_sd[g, ], model$raw_sd[1, ]) &&
      identical(model$baseline_sd[g, ], model$baseline_sd[1, ])
    flags <- c(flags, !shared & input[g, ] == input[1, ] & input[1, ] != 0)
  }
  flags
}

# Match AMCE means and, when requested, respondent SDs using a fixed training sample per attempt.
calibrate_dgp_group <- function(design, requested, sigma, latent_sigma, control) {
  meta <- effect_metadata(design)
  p <- length(requested)
  latent <- if (is.null(latent_sigma)) 0 else latent_sigma
  bound <- 1 - 1 / design$n_levels[meta$attribute_index]
  if (any(abs(requested) >= bound) || any(requested^2 + sigma^2 >= bound^2)) {
    stop("infeasible or boundary logit targets: finite coefficients cannot reach the paired-choice AMCE bounds.", call. = FALSE)
  }
  for (j in split(seq_len(p), meta$attribute_index)) {
    probabilities <- 0.5 - mean(c(0, requested[j])) + c(0, requested[j])
    if (any(probabilities <= 0 | probabilities >= 1)) {
      stop("infeasible logit AMCEs: implied marginal choice probabilities are outside (0, 1).", call. = FALSE)
    }
  }
  active_attributes <- vapply(split(requested, meta$attribute_index), function(x) any(x != 0), logical(1))
  if (sigma > 0 || latent > 0) active_attributes[] <- TRUE
  active <- which(active_attributes[meta$attribute_index])
  if (!length(active)) {
    zero <- rep(0, p)
    return(list(gamma = zero, latent_sd = zero, raw_sd = zero, baseline_sd = rep(0, length(design$n_levels)),
      diagnostics = list(converged = TRUE, iterations = 0L,
      max_residual = 0, reference = list(mean = zero, sd = zero, mcse = zero, sd_mcse = zero,
      half_width = zero, sd_half_width = zero, accepted = TRUE, contradicted = FALSE,
      batches = 0L, draws = 0L, pairs = 0L,
      method = "exact zero"))))
  }
  lv <- design$n_levels[active_attributes]
  exact_reference <- if (exact_profile_eligible(lv, control$exact_max_pairs, control$exact_max_rows)) {
    profile_reference(lv, control$calibration_pairs, control$exact_max_pairs, control$exact_max_rows)
  } else NULL
  target <- requested[active]
  q <- length(target)
  blocks <- split(seq_len(q), rep(seq_along(lv), lv - 1))
  structural_zero <- target == 0
  mean_indices <- which(target != 0)
  sd_parameter <- seq_len(q)
  for (j in blocks) {
    null <- j[target[j] == 0]
    if (length(null)) sd_parameter[null] <- null[1]
  }
  sd_group <- match(sd_parameter, unique(sd_parameter))
  sd_groups <- split(seq_len(q), sd_group)
  start <- c(4 * target[mean_indices], if (sigma > 0) rep(log(max(4 * sigma, 1e-3)), length(sd_groups)))
  iterations <- 0L
  unpack <- function(x) {
    gamma <- numeric(q)
    gamma[mean_indices] <- x[seq_along(mean_indices)]
    sd <- if (sigma > 0) exp(pmin(8, pmax(-15, x[length(mean_indices) + sd_group]))) else rep(latent, q)
    baseline <- numeric(length(lv))
    # A null level and the reference have iid Gaussian utility deviations. Subtracting the
    # reference utility induces within-attribute covariance and preserves the zero AMCE exactly.
    for (k in seq_along(blocks)) {
      j <- blocks[[k]]
      null <- j[target[j] == 0]
      if (!length(null)) next
      if (sigma > 0) baseline[k] <- sd[null[1]]
      else {
        baseline[k] <- latent / sqrt(2)
        sd[j] <- latent / sqrt(2)  # marginal SD of each coefficient remains latent_sigma
      }
    }
    list(gamma = gamma, sd = sd, baseline_sd = baseline)
  }
  for (attempt in seq_len(control$max_attempts)) {
    train <- reference_sample(lv, control, control$calibration_pairs * 2^(attempt - 1),
                               control$calibration_draws * 2^(attempt - 1), sigma > 0 || latent > 0,
                               exact_reference)
    if (!length(start)) {
      parameters <- unpack(start)
      solved <- list(converged = TRUE, residual = 0, iterations = 0L)
    } else {
      fn <- function(x) {
        par <- unpack(x)
        moment <- reference_moments(par$gamma, par$sd, train, par$baseline_sd)
        moment$sd[structural_zero] <- sqrt(pmax(0, moment$second[structural_zero]))
        sd_moments <- vapply(sd_groups, function(j) sqrt(mean(moment$sd[j]^2)), numeric(1))
        c(moment$mean[mean_indices] - target[mean_indices], if (sigma > 0) sd_moments - sigma)
      }
      if (iterations >= control$maxit) stop("logit calibration did not converge within 100 total iterations.", call. = FALSE)
      solved <- solve_amce_moments(fn, start, control$solver_tol, control$maxit - iterations)
      iterations <- iterations + solved$iterations
      if (!solved$converged) stop("logit calibration did not converge; targets may be infeasible or near a boundary (max residual ",
                                  signif(max(abs(solved$residual)), 3), ").", call. = FALSE)
      parameters <- unpack(solved$x)
      start <- solved$x
    }
    # Verification always uses fresh draws, independent of the training reference.
    verified <- verify_dgp(parameters$gamma, parameters$sd, lv, control,
                           target = target, sigma = sigma, structural_zero = structural_zero,
                           baseline_sd = parameters$baseline_sd, acceptance_margin = control$reference_margin,
                           exact_reference = exact_reference)
    if (verified$accepted) break
  }
  if (!verified$accepted) {
    if (control$reference_margin > 0) stop("reference verification could not establish the reserved calibration margin; ",
      "increase calibration/verification budgets or reduce reference_margin.", call. = FALSE)
    stop("reference verification could not establish the requested AMCE/SD precision; increase reference draws or revise the targets.", call. = FALSE)
  }
  plan <- if (control$reference_margin > 0) {
    plan_reference(verified, control, sigma, any(parameters$sd > 0))
  } else NULL
  reference <- verified
  if (verified$batches > 0L) {
    # Choose the budget before drawing; do not stop, retry, or recalibrate based on this reference.
    # A fresh substream also separates it from experiments if their seed equals the calibration seed.
    assign(".Random.seed", parallel::nextRNGSubStream(get(".Random.seed", envir = .GlobalEnv)),
           envir = .GlobalEnv)
    reference <- verify_dgp(parameters$gamma, parameters$sd, lv, control, target = target, sigma = sigma,
      structural_zero = structural_zero, baseline_sd = parameters$baseline_sd,
      fixed_batches = if (is.null(plan)) verified$batches else plan$batches,
      exact_reference = exact_reference)
  }
  reference$contradicted <- reference_contradicted(reference, target, sigma, control$tolerance)
  if (reference$contradicted) warning("The fresh final reference contradicts the requested AMCE/SD tolerance: ",
    "at least one reference interval lies wholly outside its tolerance band. ",
    "The model and reference are retained; inspect diagnostics and revisit calibration.", call. = FALSE)
  if (!is.null(plan)) {
    plan <- record_reference_precision(plan, reference)
    plan$quantities$effect_index <- active[plan$quantities$effect_index]
  }
  gamma <- sd <- numeric(p)
  baseline <- numeric(length(design$n_levels))
  gamma[active] <- parameters$gamma; sd[active] <- parameters$sd
  baseline[active_attributes] <- parameters$baseline_sd
  for (nm in c("mean", "sd", "mcse", "sd_mcse", "half_width", "sd_half_width")) {
    expanded <- numeric(p); expanded[active] <- verified[[nm]]; verified[[nm]] <- expanded
    expanded <- numeric(p); expanded[active] <- reference[[nm]]; reference[[nm]] <- expanded
  }
  result <- list(gamma = gamma, raw_sd = sd, baseline_sd = baseline,
    latent_sd = sqrt(sd^2 + baseline[meta$attribute_index]^2),
    diagnostics = list(converged = TRUE, iterations = iterations,
    attempts = attempt, max_residual = max(abs(solved$residual)), seed = control$seed,
    calibration_pairs = train$ref$pairs,
    calibration_draws = if (is.null(train$U)) 0L else 2L * nrow(train$U),
    verification = verified, reference = reference))
  if (!is.null(plan)) result$diagnostics$reference_plan <- plan
  result
}

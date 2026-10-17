# Fixed-budget population references for latent utility models. Reference streams are
# allocated independently of analyses, shortcuts, optional AMCEs and experiment streams.
truth_controls <- function(control = list()) {
  keep <- c("seed", "exact_max_pairs", "exact_max_rows", "verification_pairs",
            "verification_draws", "verification_batches")
  defaults <- c(dgp_controls()[keep], list(paired_cells = 2097152L, exact_max_cells = 5000000L))
  if (!is.list(control)) stop("truth control must be a named list.", call. = FALSE)
  if (length(control) && (is.null(names(control)) || anyNA(names(control)) || any(names(control) == "")))
    stop("every truth control must have a name.", call. = FALSE)
  if (anyDuplicated(names(control))) stop("duplicated truth control: ",
    paste(unique(names(control)[duplicated(names(control))]), collapse = ", "), ".", call. = FALSE)
  unknown <- setdiff(names(control), names(defaults))
  if (length(unknown)) stop("unknown truth control: ", paste(unknown, collapse = ", "),
    ". Valid controls: ", paste(names(defaults), collapse = ", "), ".", call. = FALSE)
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

latent_reference_streams <- function(seed, groups, batches) {
  with_reference_seed(seed, {
    root <- get(".Random.seed", envir = .GlobalEnv)
    answer <- vector("list", groups)
    for (g in seq_len(groups)) {
      state <- root
      paired <- amce <- vector("list", batches)
      for (b in seq_len(batches)) {
        state <- parallel::nextRNGSubStream(state); paired[[b]] <- state
        state <- parallel::nextRNGSubStream(state); amce[[b]] <- state
      }
      answer[[g]] <- list(root = root, paired = paired, amce = amce)
      root <- parallel::nextRNGStream(root)
    }
    answer
  })
}

latent_group_signature <- function(model, g) {
  lapply(model$terms, function(term) lapply(term, function(x) {
    if (is.matrix(x)) unname(x[g, , drop = FALSE]) else x
  }))
}

latent_truth_error <- function(message, diagnostics = list()) {
  stop(structure(list(message = message, call = NULL, diagnostics = diagnostics),
                 class = c("cj_truth_error", "error", "condition")))
}

latent_matching_analysis <- function(model, analysis, g) {
  index <- analysis$coefficient_index
  !anyNA(index) && length(index) == ncol(model$coef_mean) &&
    identical(sort(index), seq_len(ncol(model$coef_mean))) && all(model$coef_sd[g, ] == 0)
}

latent_finite_support <- function(model, g, control) {
  n_levels <- model$design$n_levels
  if (any(model$coef_sd[g, ] != 0) || any(vapply(model$terms, function(t)
      t$type == "rating" && any(t$sd[g, ] != 0), logical(1))) ||
      !exact_profile_eligible(n_levels, control$exact_max_pairs, control$exact_max_rows)) return(NULL)
  matches <- names(model$terms)[vapply(model$terms, function(t) t$type == "match", logical(1))]
  outcomes <- lapply(matches, function(nm) which(model$terms[[nm]]$probabilities[g, ] > 0))
  count <- prod(vapply(outcomes, length, integer(1)))
  cells <- exact_profile_size(n_levels)$rows * count
  if (!is.finite(cells) || cells > control$exact_max_cells) return(NULL)
  index <- if (length(outcomes)) as.matrix(expand.grid(outcomes)) else matrix(integer(), 1L, 0L)
  n <- nrow(index)
  variables <- list(matches = list(), ratings = list())
  weight <- rep(1, n)
  for (j in seq_along(matches)) {
    nm <- matches[j]; term <- model$terms[[nm]]; v <- as.integer(index[, j])
    weight <- weight * term$probabilities[g, v]
    v[v > length(model$design$levels[[term$attribute]])] <- NA_integer_
    variables$matches[[nm]] <- v
  }
  for (nm in names(model$terms)) {
    term <- model$terms[[nm]]
    if (term$type == "rating") variables$ratings[[nm]] <- matrix(term$mean[g, ], n,
      ncol(term$mean), byrow = TRUE, dimnames = list(NULL, colnames(term$mean)))
  }
  theta <- matrix(model$coef_mean[g, ], n, ncol(model$coef_mean), byrow = TRUE)
  beta <- effective_coefficients(model, theta, variables, rep(g, n))
  list(variables = variables, beta = beta, weight = weight, cells = cells,
       reference = exact_profile_reference(n_levels))
}

# A score-gap matrix per analysis column. Center at the reference before multiplying
# differences, preserving small representable gaps next to large score offsets.
latent_analysis_gaps <- function(model, analysis, variables, group_of) {
  n <- length(group_of)
  lapply(seq_len(nrow(analysis)), function(j) {
    a <- analysis[j, ]; lv <- model$design$levels[[a$attribute]]
    if (a$type == "dummy") {
      gap <- matrix(0, n, length(lv) - 1L)
      gap[, match(a$level, lv) - 1L] <- 1
      return(gap)
    }
    term <- model$terms[[a$term]]
    score <- switch(term$type,
      score = term$scores[group_of, , drop = FALSE],
      rating = variables$ratings[[a$term]],
      match = {
        out <- matrix(0, n, length(lv)); v <- variables$matches[[a$term]]
        good <- which(!is.na(v)); out[cbind(good, v[good])] <- 1; out
      })
    score[, -1L, drop = FALSE] - score[, 1L]
  })
}

latent_paired_cells <- function(model, analysis, g, n) {
  nodes <- draw_latent_respondents(model, rep(g, n))
  d <- reference_dummies(draw_profiles(model$design$n_levels, n), model$design$n_levels) -
    reference_dummies(draw_profiles(model$design$n_levels, n), model$design$n_levels)
  blocks <- split(seq_len(ncol(d)), rep(names(model$design$levels), model$design$n_levels - 1L))
  gaps <- latent_analysis_gaps(model, analysis, nodes$variables, rep(g, n))
  X <- vapply(seq_len(nrow(analysis)), function(j)
    rowSums(d[, blocks[[analysis$attribute[j]]], drop = FALSE] * gaps[[j]]), numeric(n))
  X <- matrix(X, n, nrow(analysis))
  list(X = X, generating_eta = rowSums(d * nodes$beta))
}

# Replayed in a fixed block order on every pass. No complete Monte Carlo cell set
# is retained, even when the requested budget is several million cells per batch.
latent_walk_cells <- function(model, analysis, g, streams, support, callback, block_size = 16384L) {
  if (!is.null(support)) {
    d <- support$reference$delta
    columns <- split(seq_len(ncol(d)), rep(names(model$design$levels), model$design$n_levels - 1L))
    gaps <- latent_analysis_gaps(model, analysis, support$variables, rep(g, nrow(support$beta)))
    for (i in seq_len(nrow(support$beta))) {
      for (start in seq.int(1L, nrow(d), by = block_size)) {
        rows <- start:min(nrow(d), start + block_size - 1L)
        X <- vapply(seq_len(nrow(analysis)), function(j)
          as.numeric(d[rows, columns[[analysis$attribute[j]]], drop = FALSE] %*% gaps[[j]][i, ]), numeric(length(rows)))
        callback(matrix(X, length(rows), nrow(analysis)),
          as.numeric(d[rows, , drop = FALSE] %*% support$beta[i, ]),
          support$reference$mass[rows] * support$weight[i], 1L)
      }
    }
  } else {
    n <- model$control$paired_cells
    for (b in seq_along(streams)) {
      assign(".Random.seed", streams[[b]], envir = .GlobalEnv)
      for (start in seq.int(1L, n, by = block_size)) {
        count <- min(block_size, n - start + 1L)
        cells <- latent_paired_cells(model, analysis, g, count)
        callback(cells$X, cells$generating_eta, rep(1 / n, count), b)
      }
    }
  }
  invisible(NULL)
}

latent_paired_start <- function(model, analysis, g, scales) {
  index <- analysis$coefficient_index
  zero <- list(gamma = rep(0, nrow(analysis)), method = "zero")
  if (length(index) != length(scales) || anyNA(index) || any(index < 1L) ||
      any(index > ncol(model$coef_mean))) return(zero)
  gamma <- as.numeric(model$coef_mean[g, index]) * scales
  if (any(!is.finite(gamma))) return(zero)
  list(gamma = gamma, method = "mapped coefficient means")
}

latent_paired_reference <- function(model, analysis, g, streams, support, gram) {
  J <- nrow(analysis); B <- if (is.null(support)) length(streams) else 1L
  # QR acts on design rows, never its Gram matrix. Population RMS preconditions
  # this compact streaming QR; the final QR uses the measured reference RMS.
  pop_scale <- sqrt(diag(gram)); square <- numeric(J); R <- matrix(0, 0, J)
  robust_rms <- FALSE
  latent_walk_cells(model, analysis, g, streams, support, function(X, eta, w, b) {
    Z <- sweep(X, 2L, pop_scale, "/")
    if (any(!is.finite(X)) || any(!is.finite(eta))) latent_truth_error("nonfinite paired reference cells.")
    magnitude <- abs(Z)
    if (any(!is.finite(Z)) || any(magnitude > sqrt(.Machine$double.xmax)) ||
        any(magnitude > 0 & magnitude < sqrt(.Machine$double.xmin)) || any(X != 0 & Z == 0)) {
      robust_rms <<- TRUE
    }
    if (robust_rms) return(invisible(NULL))
    square <<- square + colSums(Z^2 * w) / B
    decomposition <- qr(rbind(R, Z * sqrt(w)), LAPACK = FALSE)
    R <<- qr.R(decomposition)[, order(decomposition$pivot), drop = FALSE]
  })
  if (robust_rms) {
    # Max-factored block RMSs and a scaled hypotenuse accumulator avoid squaring
    # enormous/vanishing population-standardized cells before their tiny weights.
    scales <- numeric(J)
    latent_walk_cells(model, analysis, g, streams, support, function(X, eta, w, b) {
      block <- paired_rms(X, w / B)
      maximum <- pmax(scales, block)
      good <- maximum > 0
      next_scale <- numeric(J)
      next_scale[good] <- maximum[good] * sqrt((scales[good] / maximum[good])^2 +
                                               (block[good] / maximum[good])^2)
      scales <<- next_scale
    })
  } else scales <- pop_scale * sqrt(square)
  if (any(!is.finite(scales)) || any(scales <= 0)) latent_truth_error(
    "paired sampled reference has deficient rank; increase the reference budget.", list(scales = scales))
  if (robust_rms) {
    R <- matrix(0, 0, J)
    latent_walk_cells(model, analysis, g, streams, support, function(X, eta, w, b) {
      weighted <- sweep(X, 2L, scales, "/") * sqrt(w)
      if (any(!is.finite(weighted))) latent_truth_error("nonfinite scaled paired reference design.")
      decomposition <- qr(rbind(R, weighted), LAPACK = FALSE)
      R <<- qr.R(decomposition)[, order(decomposition$pivot), drop = FALSE]
    })
    rank <- qr(R, tol = paired_numerics()$rank_tol, LAPACK = FALSE)$rank
  } else rank <- qr(sweep(R, 2L, sqrt(square), "/"), tol = paired_numerics()$rank_tol, LAPACK = FALSE)$rank
  if (rank != J) latent_truth_error("paired sampled reference has deficient rank; increase the reference budget.",
                                   list(rank = rank, columns = J, scales = scales))
  evaluate <- function(gamma, by_batch = FALSE) {
    scores <- matrix(0, B, J); information <- matrix(0, J, J); loglik <- 0
    latent_walk_cells(model, analysis, g, streams, support, function(X, eta, w, b) {
      value <- paired_logit_evaluate(sweep(X, 2L, scales, "/"), gamma,
                                     generating_eta = eta, weights = w)
      scores[b, ] <<- scores[b, ] + value$score
      information <<- information + value$information / B
      loglik <<- loglik + value$loglik / B
    })
    out <- list(score = colMeans(scores), information = information, loglik = loglik)
    if (by_batch) out$batch_scores <- scores
    out
  }
  start <- latent_paired_start(model, analysis, g, scales)
  fit <- solve_paired_logit(evaluate, start$gamma, paired_numerics())
  warm_failure <- NULL
  if (start$method != "zero" && fit$status != "converged") {
    # A finite generating mean can still give unusable information in a misspecified
    # or highly heterogeneous analysis. Reusing it never replaces the usual safe start.
    warm_failure <- list(status = fit$status, diagnostics = fit$diagnostics)
    fit <- solve_paired_logit(evaluate, rep(0, J), paired_numerics())
  }
  diagnostic <- c(fit$diagnostics, list(status = fit$status, scales = scales, rank = rank,
    start = if (is.null(warm_failure)) start$method else "zero fallback",
    attempted_start = start$method, warm_start_failure = warm_failure,
    method = if (is.null(support)) "independent paired cells" else "exact finite support",
    rms_method = if (robust_rms) "max-factored replay" else "population-scaled RMS",
    batches = if (is.null(support)) B else 0L,
    cells_per_batch = if (is.null(support)) model$control$paired_cells else support$cells))
  diagnostic$raw_coefficients <- fit$gamma / scales
  if (fit$status != "converged") latent_truth_error(paste0("paired truth solve failed (", fit$status,
    ") for group ", if (is.null(model$groups)) "ungrouped" else model$groups[g], "."), diagnostic)
  final <- evaluate(fit$gamma, TRUE)
  correction <- as.numeric(solve(final$information, final$score)) / scales
  influences <- t(solve(final$information, t(final$batch_scores)))
  raw_vcov <- if (B > 1L) stats::cov(influences) / B else matrix(0, J, J)
  raw_vcov <- raw_vcov / scales / rep(scales, each = J)
  theta <- fit$gamma / scales
  if (any(!is.finite(c(theta, correction, raw_vcov)))) {
    diagnostic$status <- "nonfinite"
    diagnostic$newton_correction <- correction
    diagnostic$raw_reference_vcov <- raw_vcov
    latent_truth_error("paired truth has a nonfinite original-unit coefficient, correction or reference covariance.", diagnostic)
  }
  diagnostic$raw_coefficients <- theta
  diagnostic$raw_reference_vcov <- raw_vcov
  diagnostic$scaled_score <- final$score
  diagnostic$newton_correction <- correction
  diagnostic$information <- final$information
  list(value = theta, mcse = sqrt(pmax(0, diag(raw_vcov))), analytic = rep(FALSE, J),
       solver_error = abs(correction),
       roundoff_guard = 64 * .Machine$double.eps * pmax(1, abs(fit$gamma)) / scales,
       batches = if (is.null(support)) B else 0L, diagnostics = diagnostic)
}

latent_antithetic_nodes <- function(model, g, n) {
  variables <- list(matches = list(), ratings = list())
  for (nm in names(model$terms)) {
    term <- model$terms[[nm]]
    if (term$type == "match") {
      L <- ncol(term$probabilities) - 1L
      v <- sample.int(L + 1L, n, replace = TRUE, prob = term$probabilities[g, ])
      v[v > L] <- NA_integer_; variables$matches[[nm]] <- rep(v, 2L)
    }
  }
  for (nm in names(model$terms)) {
    term <- model$terms[[nm]]
    if (term$type == "rating") {
      U <- matrix(stats::rnorm(n * ncol(term$mean)), n)
      variables$ratings[[nm]] <- sweep(sweep(rbind(U, -U), 2L, term$sd[g, ], "*"), 2L, term$mean[g, ], "+")
    }
  }
  Z <- matrix(stats::rnorm(n * ncol(model$coef_mean)), n)
  theta <- sweep(sweep(rbind(Z, -Z), 2L, model$coef_sd[g, ], "*"), 2L, model$coef_mean[g, ], "+")
  effective_coefficients(model, theta, variables, rep(g, 2L * n))
}

latent_nonrandom_beta <- function(model, g) {
  all(vapply(model$terms, function(term) {
    if (all(term$coef[g, ] == 0 & term$coef_sd[g, ] == 0)) return(TRUE)
    fixed_gap <- switch(term$type, dummy = TRUE, score = TRUE,
      rating = all(term$sd[g, ] == 0), match = sum(term$probabilities[g, ] > 0) == 1L)
    zero_gap <- switch(term$type, dummy = FALSE, score = length(unique(term$scores[g, ])) == 1L,
      rating = fixed_gap && length(unique(term$mean[g, ])) == 1L,
      match = all(term$probabilities[g, -ncol(term$probabilities)] == 0))
    zero_gap || (fixed_gap && all(term$coef_sd[g, ] == 0))
  }, logical(1)))
}

latent_zero_contrast <- function(model, g, attribute, level) {
  lv <- model$design$levels[[attribute]]; l <- match(level, lv)
  terms <- model$terms[vapply(model$terms, function(t) t$attribute == attribute, logical(1))]
  all(vapply(terms, function(term) {
    if (all(term$coef[g, ] == 0 & term$coef_sd[g, ] == 0)) return(TRUE)
    switch(term$type,
      dummy = term$coef[g, l - 1L] == 0 && term$coef_sd[g, l - 1L] == 0,
      score = term$scores[g, l] == term$scores[g, 1L],
      rating = term$mean[g, l] == term$mean[g, 1L] && all(term$sd[g, c(1L, l)] == 0),
      match = all(term$probabilities[g, c(1L, l)] == 0))
  }, logical(1)))
}

# Each dummy profile difference lies in [-1, 1]. This sufficient absolute-sum
# bound rules out intermediate overflow before the unchanged forward transform,
# including cancellation patterns that could otherwise return finite wrong values.
latent_check_amce_beta <- function(beta) {
  if (any(!is.finite(beta)) || any(!is.finite(rowSums(abs(beta))))) {
    latent_truth_error("nonfinite AMCE utility arithmetic: effective coefficients or their absolute-sum bound overflow.",
      list(status = "nonfinite", stage = "amce_predictor_bound"))
  }
  invisible(NULL)
}

latent_amce_reference <- function(model, g, streams, support, certified) {
  P <- sum(model$design$n_levels - 1L); control <- model$control
  zero <- all(model$coef_mean[g, ] == 0 & model$coef_sd[g, ] == 0)
  fixed <- latent_nonrandom_beta(model, g)
  metadata <- effect_metadata(model$design)
  zero_contrast <- vapply(seq_len(P), function(j) latent_zero_contrast(model, g,
    metadata$attribute[j], metadata$level[j]), logical(1))
  if (zero) return(list(value = numeric(P), sd = numeric(P), mcse = numeric(P), sd_mcse = numeric(P),
    analytic = rep(TRUE, P), batches = 0L, diagnostics = list(method = "exact zero", batches = 0L)))
  if (!is.null(support)) {
    latent_check_amce_beta(support$beta)
    a <- conditional_amces(support$beta, support$reference)
    mu <- colSums(a * support$weight); mu[certified] <- 0
    # Centring first keeps a small SD that E[a^2] - E[a]^2 would lose to cancellation.
    sd <- if (fixed) numeric(P) else sqrt(colSums(sweep(a, 2L, mu)^2 * support$weight))
    sd[zero_contrast] <- 0
    return(list(value = mu, sd = sd, mcse = numeric(P), sd_mcse = numeric(P),
      analytic = rep(FALSE, P), batches = 0L,
      diagnostics = list(method = "exact finite support", batches = 0L, nodes = nrow(a), pairs = support$reference$pairs)))
  }
  B <- length(streams); means <- seconds <- matrix(0, B, P)
  ref_exact <- if (exact_profile_eligible(model$design$n_levels, control$exact_max_pairs, control$exact_max_rows))
    exact_profile_reference(model$design$n_levels) else NULL
  for (b in seq_len(B)) {
    assign(".Random.seed", streams[[b]], envir = .GlobalEnv)
    beta <- latent_antithetic_nodes(model, g, control$verification_draws)
    # Nonrandom coefficients make every node identical, so one row gives the same AMCEs. The nodes
    # are still drawn so that the profile draws below are unchanged.
    if (fixed) beta <- beta[1L, , drop = FALSE]
    latent_check_amce_beta(beta)
    ref <- if (is.null(ref_exact)) profile_reference(model$design$n_levels, control$verification_pairs,
      control$exact_max_pairs, control$exact_max_rows) else ref_exact
    a <- conditional_amces(beta, ref)
    second_ref <- if (ref$exact) ref else profile_reference(model$design$n_levels, control$verification_pairs,
      control$exact_max_pairs, control$exact_max_rows)
    other <- if (ref$exact) a else conditional_amces(beta, second_ref)
    means[b, ] <- colMeans((a + other) / 2)
    seconds[b, ] <- colMeans(a * other)
  }
  mu <- colMeans(means); mu[certified] <- 0
  sd <- if (fixed) numeric(P) else sqrt(pmax(0, colMeans(seconds) - mu^2))
  sd_mcse <- numeric(P)
  if (!fixed) for (j in seq_len(P)) sd_mcse[j] <- if (sd[j] > 0)
    stats::sd(seconds[, j] - 2 * mu[j] * means[, j]) / (2 * sd[j] * sqrt(B)) else Inf
  sd[zero_contrast] <- sd_mcse[zero_contrast] <- 0
  list(value = mu, sd = sd, mcse = apply(means, 2L, stats::sd) / sqrt(B), sd_mcse = sd_mcse,
       analytic = rep(FALSE, P), batches = B,
       diagnostics = list(method = if (is.null(ref_exact)) "Monte Carlo respondents and profiles" else
         "Monte Carlo respondents / exact profiles", batches = B,
         nodes = if (fixed) 1 else 2 * control$verification_draws,
         pairs = ref$pairs, batch_means = means, batch_seconds = seconds))
}

latent_amce_null <- function(model, g, attribute, level) {
  lv <- model$design$levels[[attribute]]; l <- match(level, lv)
  terms <- model$terms[vapply(model$terms, function(t) identical(t$attribute, attribute), logical(1))]
  all(vapply(terms, function(term) {
    if (all(term$coef[g, ] == 0 & term$coef_sd[g, ] == 0)) return(TRUE)
    if (length(lv) == 2L) {
      if (all(term$coef[g, ] == 0)) return(TRUE)
      return(switch(term$type, dummy = FALSE,
        score = term$scores[g, 1] == term$scores[g, l],
        match = term$probabilities[g, 1] == term$probabilities[g, l],
        rating = term$mean[g, 1] == term$mean[g, l]))
    }
    switch(term$type,
      dummy = term$coef[g, l - 1L] == 0 && term$coef_sd[g, l - 1L] == 0,
      score = term$scores[g, 1] == term$scores[g, l],
      match = term$probabilities[g, 1] == term$probabilities[g, l],
      rating = term$mean[g, 1] == term$mean[g, l] && term$sd[g, 1] == term$sd[g, l])
  }, logical(1)))
}

# One joint level permutation must match every term on an attribute. Independent
# term-by-term permutations would not preserve their joint utility/score vector.
latent_attribute_signature <- function(model, g, attribute, sign = 1) {
  L <- length(model$design$levels[[attribute]])
  rows <- matrix(numeric(), L, 0L); free <- numeric()
  for (term in model$terms) {
    if (term$attribute != attribute) next
    if (term$type == "dummy") {
      rows <- cbind(rows, sign * c(0, term$coef[g, ]), c(0, term$coef_sd[g, ]))
    } else {
      free <- c(free, sign * term$coef[g, 1L], term$coef_sd[g, 1L])
      rows <- cbind(rows, switch(term$type,
        score = term$scores[g, ], rating = cbind(term$mean[g, ], term$sd[g, ]),
        match = term$probabilities[g, seq_len(L)]))
      if (term$type == "match") free <- c(free, term$probabilities[g, L + 1L])
    }
  }
  list(rows = unname(rows), free = unname(free))
}

latent_attribute_equivalent <- function(model, g, attribute, signs = c(1, -1), fixed = integer()) {
  a <- latent_attribute_signature(model, g, attribute)
  fixed <- unique(fixed)
  if (anyNA(fixed) || any(!fixed %in% seq_len(nrow(a$rows)))) return(FALSE)
  rest <- setdiff(seq_len(nrow(a$rows)), fixed)
  sorted <- function(rows) {
    rows <- rows[rest, , drop = FALSE]
    if (!ncol(rows)) return(rows)
    rows[do.call(order, lapply(seq_len(ncol(rows)), function(j) rows[, j])), , drop = FALSE]
  }
  any(vapply(signs, function(sign) {
    b <- latent_attribute_signature(model, 1L, attribute, sign)
    identical(a$free, b$free) &&
      identical(a$rows[fixed, , drop = FALSE], b$rows[fixed, , drop = FALSE]) &&
      identical(sorted(a$rows), sorted(b$rows))
  }, logical(1)))
}

# Relabelling preserves uniform profiles. On another attribute, negating every
# utility contribution is undone by swapping its two profiles. The analysis
# transforms invertibly within that attribute, leaving the target coefficient
# unchanged. Included dummy blocks must be complete for that argument.
latent_symmetric_difference <- function(model, analysis, g, attribute, level, probability) {
  if (is.na(attribute) || !attribute %in% names(model$design$levels)) return(FALSE)
  if (!probability) {
    if (is.null(analysis)) return(FALSE)
    for (k in unique(analysis$attribute[analysis$type == "dummy"])) {
      cols <- analysis[analysis$type == "dummy" & analysis$attribute == k, , drop = FALSE]
      lv <- model$design$levels[[k]]
      if (nrow(cols) != length(lv) - 1L || !setequal(cols$level, lv[-1L]) ||
          any(cols$reference_level != lv[1L])) {
        stop("paired symmetry certification requires complete dummy blocks.", call. = FALSE)
      }
    }
  }
  lv <- model$design$levels[[attribute]]
  fixed <- if (probability || !is.na(level)) c(1L, match(level, lv)) else integer()
  latent_attribute_equivalent(model, g, attribute, signs = 1, fixed = fixed) &&
    all(vapply(setdiff(names(model$design$levels), attribute), function(k)
      latent_attribute_equivalent(model, g, k), logical(1)))
}

certify_nulls <- function(model, analysis, truth) {
  answer <- logical(nrow(truth))
  for (i in seq_len(nrow(truth))) {
    row <- truth[i, ]; g <- if (is.null(model$groups)) 1L else match(row$group_name, model$groups)
    if (row$type == "difference") {
      shared <- identical(latent_group_signature(model, g), latent_group_signature(model, 1L))
      base <- row[rep(1L, 2L), , drop = FALSE]
      base$type <- if (row$scale == "latent") "coefficient" else "amce"
      base$group_name <- base$group <- model$groups[c(g, 1L)]
      zeros <- certify_nulls(model, analysis, base)
      equal_analytic <- FALSE
      if (row$scale == "latent" && latent_matching_analysis(model, analysis, g) &&
          latent_matching_analysis(model, analysis, 1L)) {
        j <- which(analysis$term == row$term & (is.na(analysis$level) & is.na(row$level) |
          !is.na(analysis$level) & !is.na(row$level) & analysis$level == row$level))
        index <- analysis$coefficient_index[j]
        equal_analytic <- length(index) == 1L && model$coef_mean[g, index] == model$coef_mean[1L, index]
      }
      answer[i] <- shared || all(zeros) || equal_analytic ||
        latent_symmetric_difference(model, analysis, g, row$attribute, row$level,
          probability = row$scale == "probability")
      next
    }
    if (row$scale == "probability") {
      answer[i] <- latent_amce_null(model, g, row$attribute, row$level)
    } else if (latent_matching_analysis(model, analysis, g)) {
      j <- which(analysis$term == row$term & (is.na(analysis$level) & is.na(row$level) |
        !is.na(analysis$level) & !is.na(row$level) & analysis$level == row$level))
      answer[i] <- model$coef_mean[g, analysis$coefficient_index[j]] == 0
    } else {
      cols <- analysis[analysis$attribute == row$attribute, , drop = FALSE]
      if (!is.na(row$level)) {
        complete <- nrow(cols) == length(model$design$levels[[row$attribute]]) - 1L && all(cols$type == "dummy")
        answer[i] <- complete && latent_amce_null(model, g, row$attribute, row$level)
      } else {
        terms <- model$terms[vapply(model$terms, function(t) t$attribute == row$attribute, logical(1))]
        answer[i] <- length(terms) == 1L && nrow(cols) == 1L &&
          (all(terms[[1]]$coef[g, ] == 0 & terms[[1]]$coef_sd[g, ] == 0) ||
           (terms[[1]]$type == "score" && length(model$design$levels[[row$attribute]]) == 2L &&
            terms[[1]]$coef[g, 1L] == 0))
      }
    }
  }
  answer
}

latent_truth_rows <- function(model, metadata, g, result, paired, family, comparisons) {
  n <- nrow(metadata); group <- if (is.null(model$groups)) NA_character_ else model$groups[g]
  critical <- if (result$batches > 0L) stats::qt(1 - .01 / (2 * comparisons), result$batches - 1L) else 0
  data.frame(estimator = if (paired) "paired" else "amce", scale = if (paired) "latent" else "probability",
    type = if (paired) "coefficient" else "amce", term = if (paired) metadata$term else NA_character_,
    group = group, group_name = group, reference_group = NA_character_,
    metadata[c("attribute", "level", "reference_level")],
    input = if (paired) as.numeric(model$coef_mean[g, metadata$coefficient_index]) else rep(NA_real_, n),
    true_value = result$value, amce_sd = if (paired) NA_real_ else result$sd,
    reference_mcse = result$mcse, reference_half_width = result$mcse * critical,
    sd_mcse = if (paired) NA_real_ else result$sd_mcse,
    sd_half_width = if (paired) NA_real_ else result$sd_mcse * critical,
    null = FALSE, sign_resolved = FALSE, analytic = result$analytic,
    solver_error = if (paired) result$solver_error else 0,
    roundoff_guard = if (paired) result$roundoff_guard else 64 * .Machine$double.eps,
    precision_family = family, stringsAsFactors = FALSE)
}

# Only large budgets warn about runtime; smaller exploratory budgets report their size.
latent_truth_message <- function(batches, cells) {
  paste0("Computing paired population targets by simulation (", batches, " batches of ",
    format(cells, big.mark = ",", scientific = FALSE),
    " cells for each group whose targets need simulation)",
    if (as.numeric(batches) * cells >= 2^24) "; this can take several minutes" else "",
    ". Reuse the prepared model to avoid repeating this calculation.")
}

latent_truth <- function(model, analysis = NULL, amce = FALSE) {
  if (!is.logical(amce) || length(amce) != 1L || is.na(amce)) stop("`amce` must be a scalar logical.", call. = FALSE)
  if (is.null(analysis) && !amce) stop("request a paired analysis or AMCE truths.", call. = FALSE)
  model$control <- truth_controls(model$control)
  G <- nrow(model$coef_mean); B <- model$control$verification_batches
  signatures <- lapply(seq_len(G), function(g) latent_group_signature(model, g))
  shared <- vapply(seq_len(G), function(g) which(vapply(signatures[seq_len(g)], identical,
    logical(1), signatures[[g]]))[1L], integer(1))
  distinct <- length(unique(shared)); P <- sum(model$design$n_levels - 1L)
  grams <- if (is.null(analysis)) NULL else population_gram(model, analysis)
  families <- list()
  if (!is.null(analysis)) families$paired <- list(name = "paired_latent", comparisons = distinct * nrow(analysis),
    confidence = .99, analysis = analysis, base_groups = unique(shared))
  if (amce) families$amce <- list(name = "amce", comparisons = distinct * 2L * P,
    confidence = .99, quantities = c("mean", "sd"), base_groups = unique(shared))
  streams <- latent_reference_streams(model$control$seed, G, B)
  with_reference_seed(model$control$seed, {
    tables <- vector("list", G); diagnostics <- vector("list", G)
    announced <- FALSE
    for (g in seq_len(G)) {
      if (shared[g] < g) {
        tables[[g]] <- tables[[shared[g]]]
        tables[[g]]$group <- tables[[g]]$group_name <- model$groups[g]
        diagnostics[[g]] <- diagnostics[[shared[g]]]
        diagnostics[[g]]$shared_with <- shared[g]
        next
      }
      paired_analytic <- !is.null(analysis) && latent_matching_analysis(model, analysis, g)
      support <- if (amce || !paired_analytic) latent_finite_support(model, g, model$control) else NULL
      if (!announced && !is.null(analysis) && !paired_analytic && is.null(support) &&
          as.numeric(B) * model$control$paired_cells >= 1e6) {
        message(latent_truth_message(B, model$control$paired_cells))
        announced <- TRUE
      }
      rows <- list(); diagnostic <- list(streams = streams[[g]], shared_with = g)
      if (!is.null(analysis)) {
        value <- if (paired_analytic) {
          v <- as.numeric(model$coef_mean[g, analysis$coefficient_index]); n <- length(v)
          list(value = v, mcse = numeric(n), analytic = rep(TRUE, n), solver_error = numeric(n),
            roundoff_guard = numeric(n), batches = 0L,
            diagnostics = list(method = "analytic matching coding", batches = 0L, scales = sqrt(diag(grams[[g]]))))
        } else latent_paired_reference(model, analysis, g, streams[[g]]$paired, support, grams[[g]])
        rows$paired <- latent_truth_rows(model, analysis, g, value, TRUE, "paired_latent", families$paired$comparisons)
        diagnostic$paired <- value$diagnostics
      }
      if (amce) {
        meta <- effect_metadata(model$design)
        certified <- vapply(seq_len(P), function(j) latent_amce_null(model, g, meta$attribute[j], meta$level[j]), logical(1))
        value <- latent_amce_reference(model, g, streams[[g]]$amce, support, certified)
        rows$amce <- latent_truth_rows(model, meta, g, value, FALSE, "amce", families$amce$comparisons)
        diagnostic$amce <- value$diagnostics
      }
      table <- do.call(rbind, rows); rownames(table) <- NULL
      table$null <- certify_nulls(model, analysis, table)
      table$true_value[table$null] <- table$reference_mcse[table$null] <- table$reference_half_width[table$null] <- 0
      table$sign_resolved <- ifelse(table$null, NA, table$analytic | abs(table$true_value) >
        table$reference_half_width + table$solver_error + table$roundoff_guard)
      tables[[g]] <- table; diagnostics[[g]] <- diagnostic
    }
    truth <- do.call(rbind, tables)
    if (G > 1L) for (g in 2:G) {
      a <- tables[[g]]; b <- tables[[1L]]
      a$type <- "difference"; a$group <- paste(model$groups[g], "-", model$groups[1L])
      a$reference_group <- model$groups[1L]
      a$true_value <- a$true_value - b$true_value; a$input <- a$input - b$input
      if (any(!is.finite(a$true_value))) latent_truth_error("nonfinite group truth difference.")
      # Exact power-of-two scaling: large finite MCSEs cannot overflow when squared.
      s <- power2_scale(pmax(a$reference_mcse, b$reference_mcse))
      a$reference_mcse <- s * sqrt((a$reference_mcse / s)^2 + (b$reference_mcse / s)^2)
      a$reference_half_width <- a$reference_half_width + b$reference_half_width
      a$solver_error <- a$solver_error + b$solver_error
      a$roundoff_guard <- a$roundoff_guard + b$roundoff_guard
      a$analytic <- a$analytic & b$analytic
      a$null <- certify_nulls(model, analysis, a)
      a$true_value[a$null] <- a$reference_mcse[a$null] <- a$reference_half_width[a$null] <- 0
      a$amce_sd <- a$sd_mcse <- a$sd_half_width <- NA_real_
      a$sign_resolved <- ifelse(a$null, NA, a$analytic | abs(a$true_value) >
        a$reference_half_width + a$solver_error + a$roundoff_guard)
      truth <- rbind(truth, a)
    }
    rownames(truth) <- NULL
    list(truth = truth, diagnostics = list(groups = diagnostics, families = families,
      population_gram = grams, numerics = paired_numerics(), allocation = "group streams; paired substream 2b-1; AMCE substream 2b; experiments substream 0"))
  })
}

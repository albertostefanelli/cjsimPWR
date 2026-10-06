# Internal numerical rules are shared by population truth and sample fits.
paired_numerics <- function() {
  list(rank_tol = 1e-10, gram_rcond_min = 1e-12, estimability_tol = 1e-8,
       information_rcond_min = 1e-12, maxit = 100L, score_tol = 1e-10,
       step_tol = 1e-10, line_search_max_halvings = 30L, armijo = 1e-4,
       fallback_maxit = 1000L, fallback_factr = 1, fallback_pgtol = 1e-12)
}

# Scaling before squaring avoids overflow for otherwise representable column RMSs.
paired_rms <- function(X, weights = NULL) {
  if (!ncol(X)) return(numeric())
  if (!nrow(X)) return(rep(0, ncol(X)))
  if (is.null(weights)) weights <- rep(1 / nrow(X), nrow(X))
  vapply(seq_len(ncol(X)), function(j) {
    m <- max(abs(X[, j]))
    if (m == 0) 0 else m * sqrt(sum(weights * (X[, j] / m)^2))
  }, numeric(1))
}

# Both tails are evaluated directly, including when the positive tail rounds to one.
paired_logit_evaluate <- function(X, gamma, y = NULL, generating_eta = NULL,
                                   weights = NULL, p0 = NULL, q0 = NULL) {
  eta <- drop(X %*% gamma)
  p <- stats::plogis(eta)
  q <- stats::plogis(-eta)
  if (is.null(weights)) weights <- rep(1 / nrow(X), nrow(X))
  if (!is.null(y)) {
    sign <- 2 * y - 1
    residual <- sign * stats::plogis(-sign * eta)
    loglik <- sum(weights * stats::plogis(sign * eta, log.p = TRUE))
  } else {
    if (!is.null(generating_eta)) {
      p0 <- stats::plogis(generating_eta)
      q0 <- stats::plogis(-generating_eta)
    }
    residual <- p0 * q - q0 * p
    loglik <- sum(weights * (p0 * stats::plogis(eta, log.p = TRUE) +
                              q0 * stats::plogis(-eta, log.p = TRUE)))
  }
  list(score = drop(crossprod(X, weights * residual)),
       information = crossprod(X * (weights * p * q), X), loglik = loglik, eta = eta)
}

solve_paired_logit <- function(evaluate, start, numerics = paired_numerics()) {
  gamma <- as.numeric(start)
  diagnostics <- list(iteration = 0L)
  cached <- NULL
  result <- function(status) list(gamma = gamma, status = status, diagnostics = diagnostics)
  if (any(!is.finite(gamma))) return(result("nonfinite"))
  for (it in seq_len(numerics$maxit)) {
    diagnostics$iteration <- it
    value <- if (is.null(cached)) tryCatch(evaluate(gamma), error = function(e) list(error = conditionMessage(e))) else cached
    if (!is.null(value$error)) {
      diagnostics$message <- value$error
      return(result("nonfinite"))
    }
    U <- as.numeric(value$score)
    H <- value$information
    diagnostics$score <- U
    diagnostics$information <- H
    diagnostics$loglik <- value$loglik
    diagnostics$max_score <- if (length(U)) max(abs(U)) else 0
    if (length(U) != length(gamma) || any(!is.finite(c(U, H, value$loglik, value$eta))) ||
        length(value$loglik) != 1L || !identical(dim(H), c(length(gamma), length(gamma)))) {
      return(result("nonfinite"))
    }
    if (!length(gamma)) {
      diagnostics$correction <- numeric()
      diagnostics$relative_step <- 0
      diagnostics$information_rcond <- 1
      return(result("converged"))
    }
    ch <- tryCatch(chol(H), error = function(e) NULL)
    rc <- tryCatch(rcond(H), error = function(e) NA_real_)
    diagnostics$information_rcond <- rc
    if (is.null(ch) || !is.finite(rc) || rc < numerics$information_rcond_min) {
      return(result("information"))
    }
    delta <- drop(backsolve(ch, forwardsolve(t(ch), U)))
    diagnostics$correction <- delta
    diagnostics$relative_step <- max(abs(delta) / pmax(1, abs(gamma)))
    if (any(!is.finite(delta))) return(result("nonfinite"))
    if (diagnostics$max_score <= numerics$score_tol && diagnostics$relative_step <= numerics$step_tol) {
      return(result("converged"))
    }
    ok <- FALSE
    for (halving in 0:numerics$line_search_max_halvings) {
      step <- 2^-halving
      candidate <- gamma + step * delta
      next_value <- tryCatch(evaluate(candidate), error = function(e) NULL)
      if (!is.null(next_value) && length(next_value$loglik) == 1L &&
          all(is.finite(c(candidate, next_value$loglik, next_value$score,
                          next_value$information, next_value$eta))) &&
          next_value$loglik >= value$loglik + numerics$armijo * step * sum(U * delta) -
            64 * .Machine$double.eps * max(1, abs(value$loglik))) {
        ok <- TRUE
        break
      }
    }
    if (!ok) return(result("line_search"))
    gamma <- candidate
    cached <- next_value
    diagnostics$halvings <- halving
  }
  result("maxit")
}

paired_analysis <- function(design, terms, model = NULL) {
  if (!inherits(design, "cj_design")) stop("`design` must be created with conjoint_design().", call. = FALSE)
  attrs <- names(design$levels)
  if (is.null(terms)) terms <- if (is.null(model)) attrs else names(model$terms)
  if (!is.character(terms) || !length(terms) || anyNA(terms) || any(!nzchar(terms)) || anyDuplicated(terms)) {
    stop("`terms` must be a nonempty, unique character vector.", call. = FALSE)
  }
  columns <- if (!is.null(model)) vapply(model$terms, function(t) t$column, character(1)) else character()
  rows <- lapply(terms, function(term) {
    if (term %in% attrs) {
      lv <- design$levels[[term]]
      row <- data.frame(term = term, type = "dummy", attribute = term, level = lv[-1],
                        reference_level = lv[1], source_column = NA_character_,
                        coefficient_index = NA_integer_, stringsAsFactors = FALSE)
      if (!is.null(model)) {
        meta <- model$coefficient_metadata
        for (i in seq_len(nrow(row))) {
          m <- which(meta$type == "dummy" & meta$attribute == term & meta$level == row$level[i])
          if (length(m)) row$coefficient_index[i] <- meta$coefficient_index[m]
        }
      }
    } else {
      m <- match(term, columns)
      if (!is.null(model) && is.na(m)) stop("Unknown paired term '", term, "' for this model.", call. = FALSE)
      t <- if (!is.na(m)) model$terms[[m]] else NULL
      ci <- if (!is.null(t)) model$coefficient_metadata$coefficient_index[
        model$coefficient_metadata$term == names(model$terms)[m]] else NA_integer_
      row <- data.frame(term = term, type = if (is.null(t)) "score" else t$type,
                        attribute = if (is.null(t)) NA_character_ else t$attribute,
                        level = NA_character_, reference_level = NA_character_, source_column = term,
                        coefficient_index = ci, stringsAsFactors = FALSE)
    }
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$analysis_index <- seq_len(nrow(out))
  out[c("analysis_index", "term", "type", "attribute", "level", "reference_level", "source_column", "coefficient_index")]
}

paired_model_data <- function(data, design, terms, groups) {
  amce <- amce_model_data(data, design, groups)
  if (!all(c("task", "profile") %in% names(data))) {
    stop("Paired data must include task and profile identifiers.", call. = FALSE)
  }
  valid_id <- function(x) is.atomic(x) && is.null(dim(x)) && !anyNA(x) &&
    !any(as.character(x) == "") && (!is.numeric(x) || all(is.finite(x)))
  if (!valid_id(data$task) || !valid_id(data$profile)) {
    stop("`task` and `profile` must contain finite, non-missing identifiers.", call. = FALSE)
  }
  if (any(!data$y %in% c(0, 1))) stop("Paired `y` must be binary (0 or 1).", call. = FALSE)
  profile <- as.character(data$profile)
  if (any(!profile %in% c("1", "2"))) stop("Each respondent-task needs exactly one profile 1 and 2.", call. = FALSE)
  r1 <- which(profile == "1")
  r2 <- which(profile == "2")
  id <- match(data$respondent, unique(data$respondent))
  task <- match(data$task, unique(data$task))
  o1 <- order(id[r1], task[r1])
  o2 <- order(id[r2], task[r2])
  if (length(r1) != length(r2) || anyDuplicated(data.frame(id = id[r1], task = task[r1])) ||
      anyDuplicated(data.frame(id = id[r2], task = task[r2])) ||
      !identical(id[r1[o1]], id[r2[o2]]) || !identical(task[r1[o1]], task[r2[o2]])) {
    stop("Each respondent-task needs exactly one profile 1 and 2.", call. = FALSE)
  }
  r2 <- r2[o2][order(o1)]
  if (any(data$y[r1] + data$y[r2] != 1)) stop("Paired outcomes must be complementary.", call. = FALSE)
  model <- attr(data, "dgp")
  if (!identical(model$specification, "latent")) model <- NULL
  # External score columns are allowed even on simulated data; only power restricts selections.
  analysis <- paired_analysis(design, terms, if (is.null(terms)) model else NULL)
  if (!is.null(terms) && !is.null(model) && all(terms %in% c(names(design$levels), names(model$terms)))) {
    analysis <- paired_analysis(design, terms, model)
  }
  X <- vapply(seq_len(nrow(analysis)), function(j) {
    a <- analysis[j, ]
    if (a$type == "dummy") {
      as.numeric(as.character(data[[a$attribute]][r1]) == a$level) -
        as.numeric(as.character(data[[a$attribute]][r2]) == a$level)
    } else {
      x <- data[[a$source_column]]
      if (is.null(x) || !is.numeric(x) || !is.null(dim(x)) || any(!is.finite(x))) {
        stop("Selected score column '", a$source_column, "' must be finite numeric values.", call. = FALSE)
      }
      x[r1] - x[r2]
    }
  }, numeric(length(r1)))
  # vapply simplifies one-task results to a vector, even when several columns were selected.
  X <- matrix(X, nrow = length(r1), ncol = nrow(analysis))
  if (any(!is.finite(X))) stop("Paired predictor differences must be finite.", call. = FALSE)
  groups <- unique(amce$table$group_name[amce$table$type == "amce"])
  if (!length(groups)) groups <- NA_character_
  gi <- amce$group_index[r1]
  list(X = lapply(seq_along(groups), function(g) X[gi == g, , drop = FALSE]),
       y = lapply(seq_along(groups), function(g) as.integer(data$y[r1[gi == g]])),
       identifiers = lapply(seq_along(groups), function(g) {
         i <- which(gi == g)
         data.frame(respondent = data$respondent[r1[i]], task = data$task[r1[i]],
                    row1 = as.integer(r1[i]), row2 = as.integer(r2[i]))
       }), analysis = analysis, groups = groups, amce = amce)
}

paired_overlap_result <- function(ok, stage, message = NULL, error = FALSE, ...) {
  list(ok = isTRUE(ok), diagnostics = c(list(stage = stage, message = message, error = error), list(...)))
}

# Q can be supplied from the unweighted design QR, with the outcome signs applied to its rows.
paired_verify_overlap <- function(M, w, Q = NULL) {
  stage <- "weights"
  tryCatch({
    if (!length(w) || length(w) != nrow(M) || any(!is.finite(w)) || any(!is.finite(M))) {
      return(paired_overlap_result(FALSE, stage, "nonfinite or invalid weights/design", TRUE))
    }
    if (max(w) <= 0) return(paired_overlap_result(FALSE, stage, "nonpositive candidate weights"))
    w <- w / max(w)
    stage <- "projection"
    if (is.null(Q)) Q <- qr.Q(qr(M, LAPACK = FALSE))[, seq_len(ncol(M)), drop = FALSE]
    a <- w - drop(Q %*% crossprod(Q, w))
    tau <- 100 * nrow(M) * .Machine$double.eps
    if (any(!is.finite(a))) return(paired_overlap_result(FALSE, stage, "nonfinite projected weights", TRUE))
    if (min(a) <= max(tau, 1e-6 * max(abs(a)))) {
      return(paired_overlap_result(FALSE, stage, "positivity guard failed", min_weight = min(a), tolerance = tau))
    }
    stage <- "balance"
    balance <- max(abs(crossprod(M, a)) / colSums(abs(M) * a))
    if (!is.finite(balance)) return(paired_overlap_result(FALSE, stage, "nonfinite balance", TRUE))
    paired_overlap_result(balance <= tau, stage, if (balance > tau) "balance guard failed" else NULL,
                          min_weight = min(a), balance = balance, tolerance = tau)
  }, error = function(e) paired_overlap_result(FALSE, stage, conditionMessage(e), TRUE))
}

fast_overlap <- function(B, w, Q = NULL) paired_verify_overlap(B, w, Q)

quadratic_overlap <- function(B, numerics = paired_numerics()) {
  stage <- "row_scaling"
  tryCatch({
    if (any(!is.finite(B))) stop("nonfinite overlap design")
    magnitude <- apply(abs(B), 1L, max)
    A <- B[magnitude > 0, , drop = FALSE] / magnitude[magnitude > 0]
    if (!nrow(A)) stop("no nonzero overlap rows")
    A <- A / sqrt(rowSums(A^2))
    stage <- "column_qr"
    q <- qr(A, LAPACK = FALSE)
    if (q$rank != ncol(A)) stop("rank deficient overlap design")
    A <- qr.Q(q)[, seq_len(ncol(A)), drop = FALSE]
    A <- A / sqrt(rowSums(A^2))
    stage <- "optim"
    fn <- function(u) {
      value <- sum(crossprod(A, 1 + u)^2) / 2
      if (!is.finite(value)) stop("nonfinite overlap objective")
      value
    }
    gr <- function(u) {
      value <- drop(A %*% crossprod(A, 1 + u))
      if (any(!is.finite(value))) stop("nonfinite overlap gradient")
      value
    }
    fit <- stats::optim(rep(0, nrow(A)), fn, gr, method = "L-BFGS-B", lower = 0,
                        control = list(maxit = numerics$fallback_maxit,
                                       factr = numerics$fallback_factr, pgtol = numerics$fallback_pgtol))
    out <- paired_verify_overlap(A, 1 + fit$par)
    out$diagnostics$optimizer_status <- fit$convergence
    out
  }, error = function(e) paired_overlap_result(FALSE, stage, conditionMessage(e), TRUE))
}

fit_paired_block <- function(X, y) {
  numerics <- paired_numerics()
  scales <- paired_rms(X)
  failure <- list(coefficients = rep(NA_real_, ncol(X)), eta = rep(NA_real_, nrow(X)),
                  failure_reason = "nonconvergence", diagnostics = list(status = "nonfinite"))
  if (any(!is.finite(scales)) || any(scales <= 0) || !nrow(X)) return(failure)
  Xs <- sweep(X, 2L, scales, "/")
  evaluate <- function(gamma) paired_logit_evaluate(Xs, gamma, y = y)
  fit <- solve_paired_logit(evaluate, rep(0, ncol(X)), numerics)
  eta <- drop(Xs %*% fit$gamma)
  diagnostics <- fit$diagnostics
  diagnostics$status <- fit$status
  diagnostics$scales <- scales
  diagnostics$correction_original <- diagnostics$correction / scales
  diagnostics$largest_standardized_coefficient <- if (length(fit$gamma)) max(abs(fit$gamma)) else 0
  diagnostics$smallest_fitted_tail <- min(stats::plogis(-abs(eta)))
  diagnostics$overlap <- list()
  accepted <- fit$status == "converged"
  if (accepted && any(!is.finite(c(fit$gamma / scales, diagnostics$correction_original)))) {
    accepted <- FALSE
    diagnostics$status <- "nonfinite"
  }
  if (accepted && ncol(X)) {
    signs <- 2 * y - 1
    B <- Xs * signs
    fast <- tryCatch({
      Q <- qr.Q(qr(Xs, tol = numerics$rank_tol, LAPACK = FALSE))[, seq_len(ncol(Xs)), drop = FALSE] * signs
      fast_overlap(B, stats::plogis(-signs * eta), Q)
    }, error = function(e) paired_overlap_result(FALSE, "design_qr", conditionMessage(e), TRUE))
    diagnostics$overlap$fast <- fast
    accepted <- fast$ok
    if (!accepted) {
      fallback <- quadratic_overlap(B, numerics)
      diagnostics$overlap$fallback <- fallback
      accepted <- fallback$ok
    }
    if (!accepted) diagnostics$status <- "no_certificate"
  }
  list(coefficients = if (accepted) fit$gamma / scales else rep(NA_real_, ncol(X)),
       eta = if (accepted) eta else rep(NA_real_, nrow(X)),
       failure_reason = if (accepted) NA_character_ else "nonconvergence", diagnostics = diagnostics)
}

paired_reduce <- function(X, paired = TRUE) {
  p <- ncol(X)
  if (!nrow(X)) return(list(keep = integer(), X = X[, FALSE, drop = FALSE], A = matrix(0, 0, p), rank = 0L))
  scales <- if (paired) paired_rms(X) else rep(1, p)
  nonzero <- which(scales > 0 & is.finite(scales))
  if (!length(nonzero)) return(list(keep = integer(), X = X[, FALSE, drop = FALSE], A = matrix(0, 0, p), rank = 0L))
  Xs <- sweep(X[, nonzero, drop = FALSE], 2L, scales[nonzero], "/")
  q <- qr(Xs, tol = if (paired) paired_numerics()$rank_tol else 1e-7, LAPACK = FALSE)
  keep <- nonzero[q$pivot[seq_len(q$rank)]]
  A <- matrix(0, length(keep), p)
  if (length(keep)) {
    qr_reduced <- qr(sweep(X[, keep, drop = FALSE], 2L, scales[keep], "/"),
                     tol = if (paired) paired_numerics()$rank_tol else 1e-7, LAPACK = FALSE)
    A[, nonzero] <- sweep(sweep(qr.coef(qr_reduced, Xs), 1L, scales[keep], "/"), 2L, scales[nonzero], "*")
  }
  list(keep = keep, X = X[, keep, drop = FALSE], A = A, rank = length(keep))
}

paired_output_layout <- function(input, scale) {
  ng <- length(input$groups)
  if (scale == "probability") {
    out <- input$amce$table
    out$term <- NA_character_
    return(list(table = out, C = input$amce$C))
  }
  a <- input$analysis
  p <- nrow(a)
  meta <- a[c("term", "attribute", "level", "reference_level")]
  clusters <- vapply(input$identifiers, function(z) length(unique(z$respondent)), integer(1))
  out <- do.call(rbind, lapply(seq_len(ng), function(g) {
    data.frame(type = "coefficient", group = input$groups[g], group_name = input$groups[g],
               reference_group = NA_character_, meta, n_clusters_effect = clusters[g], stringsAsFactors = FALSE)
  }))
  C <- diag(ng * p)
  if (ng > 1L) for (g in 2:ng) {
    out <- rbind(out, data.frame(type = "difference", group = paste(input$groups[g], "-", input$groups[1]),
      group_name = input$groups[g], reference_group = input$groups[1], meta,
      n_clusters_effect = clusters[g] + clusters[1], stringsAsFactors = FALSE))
    C <- rbind(C, C[(g - 1L) * p + seq_len(p), , drop = FALSE] - C[seq_len(p), , drop = FALSE])
  }
  rownames(out) <- NULL
  list(table = out, C = C)
}

# Validate auxiliary models against the verified numerical solution, not glm's convergence flag.
paired_auxiliary_glm <- function(X, y, start, eta) {
  warnings <- character()
  d <- data.frame(y = y, X, check.names = FALSE)
  names(d) <- c("y", paste0("b", seq_len(ncol(X))))
  model <- withCallingHandlers(stats::glm(y ~ . - 1, data = d, family = stats::binomial(),
    start = start, control = stats::glm.control(epsilon = 1e-10, maxit = 50), x = TRUE, y = TRUE),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  gamma <- stats::coef(model)
  actual <- drop(X %*% gamma)
  n <- paired_numerics()
  value <- paired_logit_evaluate(X, gamma, y)
  ch <- chol(value$information)
  delta <- drop(backsolve(ch, forwardsolve(t(ch), value$score)))
  tails <- function(z) c(stats::plogis(z), stats::plogis(-z))
  expected_tails <- tails(eta)
  if (any(!is.finite(c(gamma, actual, delta))) ||
      max(abs(actual - eta) / pmax(1, abs(eta))) > 1e-8 ||
      any(abs(tails(actual) - expected_tails) > 1e-8 * pmax(expected_tails, .Machine$double.xmin)) ||
      rcond(value$information) < n$information_rcond_min || max(abs(value$score)) > n$score_tol ||
      max(abs(delta) / pmax(1, abs(gamma))) > n$step_tol) {
    stop("Auxiliary GLM failed agreement or stable numerical acceptance checks.", call. = FALSE)
  }
  list(model = model, warnings = warnings)
}

# Blocks carry reduced coefficient designs and output contrasts. Full-sample counts are frozen by
# the caller, so a failed unrelated group cannot alter any surviving group's finite-sample factor.
paired_covariance <- function(blocks, C, eligible, n, G, k, scale, vcov, inference, alpha) {
  nr <- nrow(C[[1]])
  V <- matrix(0, nr, nr)
  errors <- list()
  diagnostics <- list()
  failed <- integer()
  models <- vector("list", length(blocks))
  for (g in seq_along(blocks)) {
    b <- blocks[[g]]
    needed <- eligible[rowSums(abs(C[[g]][eligible, , drop = FALSE])) > 0]
    if (!length(needed)) next
    cov <- tryCatch({
      Xs <- sweep(b$fit_design, 2L, b$fit$diagnostics$scales, "/")
      eta <- b$fit$eta
      lambda <- stats::plogis(eta) * stats::plogis(-eta)
      residual <- (2 * b$y - 1) * stats::plogis(-(2 * b$y - 1) * eta)
      Hinv <- if (ncol(Xs)) chol2inv(chol(crossprod(Xs * lambda, Xs))) else matrix(0, 0, 0)
      scores <- rowsum(Xs * residual, b$identifiers$respondent, reorder = FALSE)
      if (scale == "probability") {
        Z <- b$projection$X
        derivative <- matrix(0, nrow(Z), ncol(Xs))
        derivative[b$local1, ] <- Xs * lambda
        derivative[b$local2, ] <- -Xs * lambda
        D <- crossprod(Z, derivative)
        projection_scores <- rowsum(Z * b$projection_residual, b$profile_id, reorder = FALSE)
        # Match respondent rows explicitly; profile input order may differ from task order.
        score_index <- match(rownames(projection_scores), rownames(scores))
        combined <- projection_scores + scores[score_index, , drop = FALSE] %*% Hinv %*% t(D)
        influences <- combined %*% chol2inv(chol(crossprod(Z)))
        list(V = crossprod(influences))
      } else if (vcov == "CR1") {
        influence <- sweep(scores %*% Hinv, 2L, b$fit$diagnostics$scales, "/")
        list(V = crossprod(influence))
      } else {
        aux <- paired_auxiliary_glm(Xs, b$y, b$fit$coefficients * b$fit$diagnostics$scales, eta)
        cov <- clubSandwich::vcovCR(aux$model, cluster = b$identifiers$respondent, type = "CR2")
        models[[g]] <- list(aux = aux, covariance = cov, X = Xs)
        diagnostics[[g]] <- aux$warnings
        list(V = sweep(sweep(as.matrix(cov), 1L, b$fit$diagnostics$scales, "/"),
                       2L, b$fit$diagnostics$scales, "/"))
      }
    }, error = function(e) list(error = conditionMessage(e)))
    if (!is.null(cov$error)) {
      failed <- union(failed, needed)
      errors[[as.character(g)]] <- cov$error
    } else {
      if (vcov == "CR1") cov$V <- cov$V * G / (G - 1) * (n - 1) / (n - k)
      V <- V + C[[g]] %*% cov$V %*% t(C[[g]])
    }
  }
  df <- rep(if (inference == "t") G - 1 else Inf, nr)
  if (vcov == "CR2" && inference == "Satterthwaite") {
    for (g in seq_along(blocks)) {
      rows <- setdiff(eligible, failed)
      support <- vapply(seq_len(nr), function(i) {
        active <- which(vapply(C, function(cg) any(cg[i, ] != 0), logical(1)))
        length(active) && max(active) == g
      }, logical(1))
      rows <- rows[support[rows]]
      if (!length(rows)) next
      # Single-group and difference rows need different supporting working models.
      support_sets <- split(rows, vapply(rows, function(i) paste(which(vapply(C,
        function(cg) any(cg[i, ] != 0), logical(1))), collapse = ","), character(1)))
      for (r in support_sets) {
        involved <- which(vapply(C, function(cg) any(cg[r[1], ] != 0), logical(1)))
        tested <- tryCatch({
          if (length(involved) == 1L) {
            a <- involved[1]
            md <- models[[a]]
            contrast <- sweep(C[[a]][r, , drop = FALSE], 2L, blocks[[a]]$fit$diagnostics$scales, "/")
          } else {
            a <- involved[1]; b <- involved[2]
            Xa <- models[[a]]$X; Xb <- models[[b]]$X
            joint <- rbind(cbind(Xa, matrix(0, nrow(Xa), ncol(Xb))),
                           cbind(matrix(0, nrow(Xb), ncol(Xa)), Xb))
            yy <- c(blocks[[a]]$y, blocks[[b]]$y)
            start <- c(blocks[[a]]$fit$coefficients * blocks[[a]]$fit$diagnostics$scales,
                       blocks[[b]]$fit$coefficients * blocks[[b]]$fit$diagnostics$scales)
            eta <- c(blocks[[a]]$fit$eta, blocks[[b]]$fit$eta)
            aux <- paired_auxiliary_glm(joint, yy, start, eta)
            cluster <- c(as.character(blocks[[a]]$identifiers$respondent), as.character(blocks[[b]]$identifiers$respondent))
            md <- list(aux = aux, covariance = clubSandwich::vcovCR(aux$model, cluster = cluster, type = "CR2"))
            contrast <- cbind(sweep(C[[a]][r, , drop = FALSE], 2L, blocks[[a]]$fit$diagnostics$scales, "/"),
                              sweep(C[[b]][r, , drop = FALSE], 2L, blocks[[b]]$fit$diagnostics$scales, "/"))
          }
          clubSandwich::linear_contrast(md$aux$model, vcov = md$covariance, contrasts = contrast,
                                        level = 1 - alpha, test = "Satterthwaite")$df
        }, error = function(e) list(error = conditionMessage(e)))
        if (is.list(tested) && !is.null(tested$error)) {
          failed <- union(failed, r)
          errors[[paste(involved, collapse = ",")]] <- tested$error
        } else df[r] <- tested
      }
    }
  }
  list(V = V, df = df, failed = failed, errors = errors, diagnostics = diagnostics)
}

#' Estimate a paired logit with respondent-clustered inference
#'
#' Fits a no-intercept logit to profile-1 minus profile-2 predictors. Positive coefficients favour
#' profile 1. Probability output projects fitted probabilities onto the ordinary AMCE design and
#' includes uncertainty from the fitted logit and the sampled profiles/respondents.
#'
#' @param data Profile rows with binary complementary outcomes, respondent and task identifiers,
#'   exactly one profile 1 and 2 per task, and the design's attribute columns.
#' @param design A design returned by [conjoint_design()].
#' @param terms Nonempty unique character vector. Attributes select their non-reference dummies;
#'   other names select finite numeric score columns. NULL uses an attached latent model's terms,
#'   or all design attributes.
#' @param groups Group names in desired order, as in [estimate_amce()]. If omitted, factor group
#'   levels or the first appearance of character groups determines the order and reference group.
#'   Use `groups = model$groups` to retain a prepared model's order after shuffling data rows.
#' @param vcov Respondent-clustered covariance: `"CR1"` or `"CR2"`. CR2 requires clubSandwich and
#'   is available only on the latent scale.
#' @param inference `"normal"`, `"t"` (all respondents minus one degrees of freedom), or
#'   `"Satterthwaite"` (CR2 only). Defaults match [estimate_amce()].
#' @param alpha Two-sided significance level.
#' @param scale `"latent"` for logit coefficients or `"probability"` for AMCE projections.
#'
#' @return A data frame of group effects followed by group-minus-first-group differences, with
#'   estimate, standard error, inference, `term`, `scale`, and the diagnostics of [estimate_amce()].
#'   Covariance in output order is attached as `vcov`. Failed rows retain a `failure_reason`;
#'   `nonconvergence` includes a failed numerical fit or unverified finite MLE. Solver and overlap
#'   details are attached as `diagnostics`. Group failures do not invalidate unrelated groups.
#' @details Probability output has an AMCE interpretation under independent uniform assignment;
#'   otherwise it is the stated regression projection. Probability CR1 includes both sources of
#'   uncertainty. No intercept is added to the paired logit. Labels are never interpreted as formulas.
#' @seealso [utility_model()], [estimate_amce()], [power_sim()]
#' @md
#' @export
#' @examples
#' design <- conjoint_design(list(quality = c("low", "high"), democracy = c("D+", "D-")))
#' model <- utility_model(design, list(term_score("quality", c(0, 2), .3),
#'                                     term_dummy("democracy", -.8)))
#' set.seed(1)
#' data <- simulate_experiment(design, units = 100, n_tasks = 3, model = model)
#' estimate_paired(data, design)
#' estimate_paired(data, design, scale = "probability")
estimate_paired <- function(data, design, terms = NULL, groups = NULL, vcov = c("CR1", "CR2"),
                            inference = NULL, alpha = 0.05, scale = c("latent", "probability")) {
  vcov <- match.arg(vcov)
  scale <- match.arg(scale)
  inference <- check_inference(vcov, inference, alpha)
  if (vcov == "CR2" && scale == "probability") stop("Paired probability output requires CR1 covariance.", call. = FALSE)
  input <- paired_model_data(data, design, terms, groups)
  layout <- paired_output_layout(input, scale)
  out <- layout$table
  ng <- length(input$groups)
  J <- nrow(input$analysis)
  P <- sum(lengths(design$levels) - 1L) + 1L
  blocks <- lapply(seq_len(ng), function(g) {
    reduced <- paired_reduce(input$X[[g]])
    ids <- input$identifiers[[g]]
    rows <- which(input$amce$group_index == g)
    projection <- paired_reduce(input$amce$X[rows, (g - 1L) * P + seq_len(P), drop = FALSE], paired = FALSE)
    list(reduced = reduced, fit = NULL, fit_design = reduced$X, y = input$y[[g]], identifiers = ids,
         profile_rows = rows, profile_id = data$respondent[rows],
         local1 = match(ids$row1, rows), local2 = match(ids$row2, rows), projection = projection)
  })
  # Counts include all groups even when their subsequent fit fails.
  G <- length(unique(data$respondent))
  n <- if (scale == "latent") sum(lengths(input$y)) else nrow(data)
  k <- sum(vapply(blocks, function(b) if (scale == "latent") b$reduced$rank else b$projection$rank, integer(1)))
  C <- lapply(seq_len(ng), function(g) {
    columns <- if (scale == "latent") (g - 1L) * J + seq_len(J) else (g - 1L) * P + seq_len(P)
    red <- if (scale == "latent") blocks[[g]]$reduced else blocks[[g]]$projection
    layout$C[, columns[red$keep], drop = FALSE]
  })
  out$estimate <- 0
  out$failure_reason <- NA_character_
  numerics <- paired_numerics()
  for (g in seq_len(ng)) {
    b <- blocks[[g]]
    columns <- if (scale == "latent") (g - 1L) * J + seq_len(J) else (g - 1L) * P + seq_len(P)
    full_C <- layout$C[, columns, drop = FALSE]
    involved <- rowSums(abs(full_C)) > 0
    red <- if (scale == "latent") b$reduced else b$projection
    estimable <- apply(abs(full_C - C[[g]] %*% red$A), 1L, max) < numerics$estimability_tol
    out$failure_reason[involved & !estimable] <- "non_estimable"
    if (!nrow(b$fit_design)) next
    b$fit <- fit_paired_block(b$fit_design, b$y)
    if (!is.na(b$fit$failure_reason)) {
      out$failure_reason[involved & (is.na(out$failure_reason) |
        out$failure_reason == "insufficient_clusters")] <- b$fit$failure_reason
    } else if (scale == "latent") {
      out$estimate <- out$estimate + drop(C[[g]] %*% b$fit$coefficients)
    } else {
      prediction <- rep(NA_real_, length(b$profile_rows))
      prediction[b$local1] <- stats::plogis(b$fit$eta)
      prediction[b$local2] <- stats::plogis(-b$fit$eta)
      projected <- stats::lm.fit(b$projection$X, prediction)
      if (!ncol(b$fit_design)) {
        # With no varying paired predictors, probabilities are identically one half. Remove
        # least-squares roundoff so a deterministic projection cannot acquire spurious inference.
        projected$coefficients[] <- 0
        projected$coefficients[b$projection$keep == 1L] <- .5
        projected$residuals[] <- 0
      }
      b$projection_residual <- projected$residuals
      out$estimate <- out$estimate + drop(C[[g]] %*% projected$coefficients)
    }
    if (length(unique(b$identifiers$respondent)) < 2L) {
      out$failure_reason[involved & is.na(out$failure_reason)] <- "insufficient_clusters"
    }
    blocks[[g]] <- b
  }
  out$estimate[out$failure_reason %in% c("non_estimable", "nonconvergence")] <- NA_real_
  if (n <= k) out$failure_reason[is.na(out$failure_reason)] <- "no_residual_df"
  for (nm in c("std.error", "df", "statistic", "p.value", "conf.low", "conf.high")) out[[nm]] <- NA_real_
  out$scale <- scale
  out$vcov <- vcov
  out$inference <- inference
  out$n_clusters <- G
  out$rank <- k
  eligible <- which(is.na(out$failure_reason))
  V <- matrix(NA_real_, nrow(out), nrow(out))
  covariance_diagnostics <- list()
  if (length(eligible)) {
    cv <- paired_covariance(blocks, C, eligible, n, G, k, scale, vcov, inference, alpha)
    V <- cv$V
    out$failure_reason[cv$failed] <- "covariance_failure"
    if (length(cv$errors)) attr(out, "covariance_error") <- cv$errors
    covariance_diagnostics <- cv$diagnostics
    eligible <- setdiff(eligible, cv$failed)
    variance <- diag(V)[eligible]
    valid_var <- is.finite(variance) & variance > 0
    valid_df <- !is.na(cv$df[eligible]) & cv$df[eligible] > 0
    out$failure_reason[eligible[!valid_var]] <- "invalid_variance"
    out$failure_reason[eligible[valid_var & !valid_df]] <- "invalid_df"
    good <- eligible[valid_var & valid_df]
    out$std.error[good] <- sqrt(diag(V)[good])
    out$df[good] <- cv$df[good]
    out$statistic[good] <- out$estimate[good] / out$std.error[good]
    out$p.value[good] <- 2 * stats::pt(abs(out$statistic[good]), df = out$df[good], lower.tail = FALSE)
    critical <- stats::qt(1 - alpha / 2, df = out$df[good])
    out$conf.low[good] <- out$estimate[good] - critical * out$std.error[good]
    out$conf.high[good] <- out$estimate[good] + critical * out$std.error[good]
  }
  out$status <- ifelse(is.na(out$failure_reason), "ok", "failed")
  failed <- out$status == "failed"
  V[failed, ] <- NA_real_
  V[, failed] <- NA_real_
  attr(out, "vcov") <- V
  attr(out, "diagnostics") <- list(numerics = numerics,
    groups = lapply(blocks, function(b) if (is.null(b$fit)) list(status = "absent") else b$fit$diagnostics),
    covariance = covariance_diagnostics)
  out
}

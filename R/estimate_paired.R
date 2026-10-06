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

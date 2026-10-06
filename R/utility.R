#' Latent utility terms
#'
#' Describe a contribution to profile utility. Inputs are recorded here and validated against the
#' design and groups by [utility_model()]. A positive coefficient favours a profile with a higher
#' score. Respondent coefficients and respondent scores remain fixed across that respondent's tasks.
#'
#' @param attribute name of one design attribute.
#' @param coef coefficient means: one value per non-reference level for a dummy term, otherwise one
#'   unnamed scalar. A named list supplies separate values for each group.
#' @param coef_sd coefficient standard deviations, with the same shape as `coef`. An unnamed scalar
#'   may be repeated across a dummy term's coefficients. The default is zero.
#' @param scores finite, nonconstant fixed scores for all levels, including the reference.
#' @param share probabilities of matching each level, including the reference. Their sum must not
#'   exceed one (apart from roundoff of at most `1e-12`, which is normalized). The remaining probability
#'   means no match. A respondent matches at most one level.
#' @param mean,sd means and standard deviations of independent normal respondent ratings at every
#'   level. An unnamed scalar `sd` may be repeated across levels. Ratings are not rounded or clipped.
#'
#' @details Level vectors follow design order when unnamed; names must match the complete required
#'   set of level labels and are reordered to design order. Each numeric argument may instead be a
#'   list named by exactly the model's groups. Only unnamed scalar standard deviations are recycled.
#'   Different terms' respondent variables and coefficient deviations are independent within group.
#'
#' @return A utility term specification for [utility_model()].
#' @seealso [utility_model()], [estimate_paired()]
#' @export
#' @md
#' @examples
#' term_dummy("democracy", coef = -0.8, coef_sd = 0.3)
#' term_score("quality", scores = c(0, 1, 2), coef = 0.4)
#' term_match("party", share = c(0.4, 0.3, 0.2), coef = 0.9)
#' term_rating("policy", mean = c(4, 6), sd = 2, coef = 0.4)
term_dummy <- function(attribute, coef, coef_sd = 0) {
  structure(list(type = "dummy", attribute = attribute, coef = coef, coef_sd = coef_sd),
            class = "cj_utility_term")
}

#' @rdname term_dummy
#' @export
term_score <- function(attribute, scores, coef, coef_sd = 0) {
  structure(list(type = "score", attribute = attribute, scores = scores, coef = coef,
                 coef_sd = coef_sd), class = "cj_utility_term")
}

#' @rdname term_dummy
#' @export
term_match <- function(attribute, share, coef, coef_sd = 0) {
  structure(list(type = "match", attribute = attribute, share = share, coef = coef,
                 coef_sd = coef_sd), class = "cj_utility_term")
}

#' @rdname term_dummy
#' @export
term_rating <- function(attribute, mean, sd, coef, coef_sd = 0) {
  structure(list(type = "rating", attribute = attribute, mean = mean, sd = sd, coef = coef,
                 coef_sd = coef_sd), class = "cj_utility_term")
}

#' Prepare a latent utility model
#'
#' Specify coefficients directly on the logit utility scale and compute population truths for paired
#' logistic regression, with optional AMCE truths. Profiles are independently and uniformly randomized.
#'
#' @param design a `cj_design` from [conjoint_design()].
#' @param terms nonempty list of terms from [term_dummy()], [term_score()], [term_match()] or
#'   [term_rating()]. Every attribute needs at least one term and at most one dummy term. Unnamed
#'   terms use the attribute name for dummies and `attribute_score`, `attribute_match` or
#'   `attribute_rating` otherwise. List names may override non-dummy names; they must be unique and
#'   differ from all attribute names and reserved simulation columns.
#' @param groups `NULL`, or unique respondent-group names. Named numeric-argument lists must match
#'   these groups exactly; ordinary vectors are shared across groups.
#' @param amce whether to compute and cache AMCE means and respondent-level standard deviations.
#' @param control named integration controls: `seed`, `exact_max_pairs`, `exact_max_rows`,
#'   `verification_pairs`, `verification_draws`, `verification_batches`, `paired_cells` and
#'   `exact_max_cells`. Budgets are fixed; inspect `diagnostics` and reference half-widths for precision.
#'   The default `paired_cells` is 2,097,152 per group per batch. Numerical truth construction can
#'   take several minutes; reuse the prepared model across power analyses.
#'
#' @details The default paired analysis contains every utility term and must be identified in every
#'   group. Fixed coefficients with exactly matching analysis coding have paired targets equal to their
#'   input means. Under heterogeneity or a changed analysis, the paired target is a population logistic
#'   projection. AMCE truths are on the probability scale. A certified zero mean does not imply zero
#'   respondent-level variation. Truth calculations preserve the caller's random-number state and use
#'   reference streams separate from simulated experiments.
#'
#' @return A prepared `cj_dgp` with `specification = "latent"`, normalized `terms`, coefficient
#'   `parameters`, population `truth`, numerical `diagnostics` and resolved `control`. Pass it to
#'   [simulate_experiment()] or [power_sim()]. Simulation adds a per-profile score column for each
#'   non-dummy term. These columns reveal scores at shown levels, not ratings of unseen levels.
#' @seealso [estimate_paired()], [simulate_experiment()], [power_sim()],
#'   [paired-logit guide](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/paired_logit.md)
#' @export
#' @md
#' @examples
#' design <- conjoint_design(list(quality = c("low", "high"), democracy = c("D+", "D-")))
#' model <- utility_model(design, list(term_score("quality", c(0, 2), 0.3),
#'                                     term_dummy("democracy", -0.8)))
#' model$truth
utility_model <- function(design, terms, groups = NULL, amce = FALSE, control = list()) {
  if (!is.logical(amce) || length(amce) != 1L || is.na(amce)) {
    stop("`amce` must be one non-missing logical value.", call. = FALSE)
  }
  model <- latent_specification(design, terms, groups, control)
  analysis <- paired_analysis(design, names(model$terms), model)
  gram <- population_gram(model, analysis)
  result <- latent_truth(model, analysis, amce)
  model$truth <- result$truth
  model$diagnostics <- result$diagnostics
  if (is.null(model$diagnostics$population_gram)) model$diagnostics$population_gram <- gram
  class(model) <- "cj_dgp"
  model
}

# Constructors deliberately defer design-dependent validation to this normalization boundary.
normalize_terms <- function(design, terms, groups) {
  if (!is.list(terms) || !length(terms)) {
    stop("`terms` must be a nonempty list of utility terms.", call. = FALSE)
  }
  attrs <- names(design$levels)
  supplied <- names(terms)
  if (is.null(supplied)) supplied <- rep("", length(terms))
  if (anyNA(supplied)) stop("utility term names must not be missing.", call. = FALSE)
  resolved <- character(length(terms))
  normalized <- vector("list", length(terms))
  for (i in seq_along(terms)) {
    term <- terms[[i]]
    if (!inherits(term, "cj_utility_term") || !is.list(term) || length(term$type) != 1L ||
        !term$type %in% c("dummy", "score", "match", "rating")) {
      stop("term ", i, " must be created with a utility term constructor.", call. = FALSE)
    }
    attr <- term$attribute
    if (!is.character(attr) || length(attr) != 1L || is.na(attr) || !attr %in% attrs) {
      stop("term ", i, " must name an attribute in the design: ", paste(attrs, collapse = ", "),
           ".", call. = FALSE)
    }
    default <- if (term$type == "dummy") attr else paste0(attr, "_", term$type)
    nm <- if (supplied[i] == "") default else supplied[i]
    if (term$type == "dummy" && nm != attr) {
      stop("dummy term '", nm, "' must be named for its attribute '", attr, "'.", call. = FALSE)
    }
    if (term$type != "dummy" && nm %in% c(attrs, "group", "respondent", "task", "profile", "y")) {
      stop("term '", nm, "' for attribute '", attr,
           "' conflicts with an attribute or reserved data column.", call. = FALSE)
    }
    resolved[i] <- nm
    lv <- design$levels[[attr]]
    coef_levels <- if (term$type == "dummy") lv[-1L] else NULL
    normalized[[i]] <- list(type = term$type, attribute = attr,
                            column = if (term$type == "dummy") NA_character_ else nm,
                            coef = latent_numeric_input(term$coef, "coef", nm, attr, groups,
                                                        coef_levels, FALSE),
                            coef_sd = latent_numeric_input(term$coef_sd, "coef_sd", nm, attr, groups,
                                                           coef_levels, TRUE))
    if (term$type == "score") {
      scores <- latent_numeric_input(term$scores, "scores", nm, attr, groups, lv, FALSE)
      for (g in seq_len(nrow(scores))) if (length(unique(scores[g, ])) < 2L) {
        latent_input_error("scores", nm, attr, groups, g, lv, "must be nonconstant")
      }
      normalized[[i]]$scores <- scores
    } else if (term$type == "match") {
      shares <- latent_numeric_input(term$share, "share", nm, attr, groups, lv, FALSE)
      probabilities <- matrix(0, nrow(shares), length(lv) + 1L,
                              dimnames = list(groups, NULL))
      for (g in seq_len(nrow(shares))) {
        s <- shares[g, ]
        if (any(s < 0 | s > 1) || sum(s) > 1 + 1e-12) {
          latent_input_error("share", nm, attr, groups, g, lv,
                             "must lie in [0, 1] and sum to at most 1 (tolerance 1e-12)")
        }
        total <- sum(s)
        if (total > 1) s <- s / total
        probabilities[g, ] <- c(s, if (total > 1) 0 else 1 - total)
      }
      normalized[[i]]$probabilities <- probabilities
    } else if (term$type == "rating") {
      normalized[[i]]$mean <- latent_numeric_input(term$mean, "mean", nm, attr, groups, lv, FALSE)
      normalized[[i]]$sd <- latent_numeric_input(term$sd, "sd", nm, attr, groups, lv, TRUE)
    }
  }
  if (anyDuplicated(resolved)) stop("resolved utility term names must be unique.", call. = FALSE)
  names(normalized) <- resolved
  covered <- vapply(normalized, function(x) x$attribute, character(1))
  if (any(!attrs %in% covered)) {
    stop("every design attribute requires a utility term; missing: ",
         paste(attrs[!attrs %in% covered], collapse = ", "), ".", call. = FALSE)
  }
  dummy_attrs <- covered[vapply(normalized, function(x) x$type == "dummy", logical(1))]
  if (anyDuplicated(dummy_attrs)) {
    stop("each attribute permits at most one dummy term.", call. = FALSE)
  }
  normalized
}

latent_input_error <- function(argument, term, attribute, groups, g, labels, problem) {
  group <- if (is.null(groups)) "ungrouped" else groups[g]
  expected <- if (is.null(labels)) "one unnamed scalar" else {
    paste0("levels [", paste(labels, collapse = ", "), "]")
  }
  stop("`", argument, "` for term '", term, "', group '", group, "', attribute '", attribute,
       "' ", problem, "; expected ", expected, ".", call. = FALSE)
}

# Normalize one numeric argument; unlike ordinary R arithmetic, never recycle a non-SD input.
latent_numeric_input <- function(x, argument, term, attribute, groups, labels, is_sd) {
  ng <- max(1L, length(groups))
  q <- if (is.null(labels)) 1L else length(labels)
  if (is.list(x)) {
    nm <- names(x)
    if (is.null(groups) || is.null(nm) || length(x) != ng || anyNA(nm) || anyDuplicated(nm) ||
        !setequal(nm, groups)) {
      latent_input_error(argument, term, attribute, groups, 1L, labels,
                         "must be a numeric vector or a list named by exactly the model groups")
    }
    rows <- x[groups]
  } else rows <- rep(list(x), ng)
  values <- lapply(seq_len(ng), function(g) {
    value <- rows[[g]]
    if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) || !all(is.finite(value))) {
      latent_input_error(argument, term, attribute, groups, g, labels, "must contain finite numbers")
    }
    if (is_sd && length(value) == 1L && is.null(names(value))) value <- rep(value, q)
    if (length(value) != q) {
      latent_input_error(argument, term, attribute, groups, g, labels, "has the wrong length")
    }
    nm <- names(value)
    if (is.null(labels)) {
      if (!is.null(nm)) latent_input_error(argument, term, attribute, groups, g, labels, "must be unnamed")
    } else if (!is.null(nm)) {
      if (anyNA(nm) || anyDuplicated(nm) || !setequal(nm, labels)) {
        latent_input_error(argument, term, attribute, groups, g, labels, "has incorrect level names")
      }
      value <- value[labels]
    }
    if (is_sd && any(value < 0)) {
      latent_input_error(argument, term, attribute, groups, g, labels, "must be nonnegative")
    }
    as.numeric(value)
  })
  result <- matrix(unlist(values, use.names = FALSE), nrow = ng, byrow = TRUE)
  dimnames(result) <- list(groups, labels)
  result
}

latent_specification <- function(design, terms, groups = NULL, control = list()) {
  if (!inherits(design, "cj_design")) {
    stop("`design` must be created with conjoint_design().", call. = FALSE)
  }
  groups <- check_groups(groups)
  terms <- normalize_terms(design, terms, groups)
  metadata <- do.call(rbind, lapply(names(terms), function(nm) {
    term <- terms[[nm]]
    lv <- design$levels[[term$attribute]]
    data.frame(term = nm, type = term$type, attribute = term$attribute,
               level = if (term$type == "dummy") lv[-1L] else NA_character_,
               reference_level = if (term$type == "dummy") lv[1L] else NA_character_,
               stringsAsFactors = FALSE)
  }))
  rownames(metadata) <- NULL
  metadata <- cbind(coefficient_index = seq_len(nrow(metadata)), metadata)
  means <- do.call(cbind, lapply(terms, function(term) term$coef))
  sds <- do.call(cbind, lapply(terms, function(term) term$coef_sd))
  dimnames(means) <- dimnames(sds) <- list(groups, paste0("c", seq_len(nrow(metadata))))
  parameters <- do.call(rbind, lapply(seq_len(nrow(means)), function(g) {
    data.frame(group = if (is.null(groups)) NA_character_ else groups[g],
               metadata[, c("term", "type", "attribute", "level", "reference_level"), drop = FALSE],
               latent_mean = unname(means[g, ]), latent_sd = unname(sds[g, ]),
               stringsAsFactors = FALSE)
  }))
  rownames(parameters) <- NULL
  list(design = design, groups = groups, dgp = "logit", specification = "latent", terms = terms,
       coefficient_metadata = metadata, coef_mean = means, coef_sd = sds, parameters = parameters,
       control = truth_controls(control))
}

# Exact second moments of selected profile-score differences. Center means before multiplying,
# and compute respondent covariances separately so large common score offsets cannot erase variance.
population_gram <- function(model, analysis) {
  design <- model$design
  ng <- nrow(model$coef_mean)
  j <- nrow(analysis)
  result <- lapply(seq_len(ng), function(g) {
    means <- vector("list", j)
    covariance <- vector("list", j)
    for (a in seq_len(j)) {
      row <- analysis[a, ]
      lv <- design$levels[[row$attribute]]
      if (row$type == "dummy") {
        mu <- as.numeric(lv == row$level)
      } else {
        term <- model$terms[[row$source_column]]
        if (is.null(term)) stop("analysis score column is not generated by this model.", call. = FALSE)
        if (term$type == "score") mu <- term$scores[g, ]
        else if (term$type == "rating") {
          mu <- term$mean[g, ]
          covariance[[a]] <- diag(as.numeric(term$sd[g, ]^2), nrow = length(lv))
        } else {
          mu <- term$probabilities[g, seq_along(lv)]
          covariance[[a]] <- diag(as.numeric(mu), nrow = length(lv)) - tcrossprod(mu)
        }
      }
      mu <- mu - mu[1L]
      means[[a]] <- mu - mean(mu)
    }
    gram <- matrix(0, j, j)
    for (a in seq_len(j)) for (b in seq_len(a)) {
      if (analysis$attribute[a] != analysis$attribute[b]) next
      value <- mean(means[[a]] * means[[b]])
      if (a == b && !is.null(covariance[[a]])) {
        cv <- covariance[[a]]
        levels <- nrow(cv)
        value <- value + sum(diag(cv)) / levels - sum(cv) / levels^2
      }
      gram[a, b] <- gram[b, a] <- 2 * value
    }
    labels <- ifelse(analysis$type == "dummy", paste0(analysis$term, "[", analysis$level, "]"),
                     analysis$term)
    dimnames(gram) <- list(labels, labels)
    diagonal <- diag(gram)
    scale <- sqrt(diagonal)
    condition <- 0
    scaled <- gram
    good <- all(is.finite(gram)) && all(diagonal > 0)
    if (good) {
      scaled <- sweep(sweep(gram, 1L, scale, "/"), 2L, scale, "/")
      scaled <- (scaled + t(scaled)) / 2
      condition <- tryCatch(rcond(scaled), error = function(e) 0)
      good <- is.finite(condition) && condition >= paired_numerics()$gram_rcond_min &&
        !inherits(try(chol(scaled), silent = TRUE), "try-error")
    }
    if (!good) {
      group <- if (is.null(model$groups)) "ungrouped" else model$groups[g]
      stop("analysis not identified or numerically ill-conditioned for group '", group,
           "', columns ", paste(labels, collapse = ", "), "; scaled Gram rcond = ",
           format(condition, digits = 6), ".", call. = FALSE)
    }
    attr(gram, "scale") <- scale
    attr(gram, "rcond") <- condition
    attr(gram, "scaled_gram") <- scaled
    gram
  })
  if (!is.null(model$groups)) names(result) <- model$groups
  result
}

# Full level scores for one non-dummy term, always with respondent rows and design-level columns.
latent_term_scores <- function(model, term, variables, group_of) {
  if (is.character(term)) term <- model$terms[[term]]
  levels <- model$design$levels[[term$attribute]]
  n <- length(group_of)
  if (term$type == "score") return(term$scores[group_of, , drop = FALSE])
  if (term$type == "rating") return(variables$ratings[[term$column]])
  if (term$type == "match") {
    index <- variables$matches[[term$column]]
    scores <- matrix(0, n, length(levels), dimnames = list(NULL, levels))
    has <- !is.na(index)
    scores[cbind(which(has), index[has])] <- 1
    return(scores)
  }
  stop("full level scores require a non-dummy term.", call. = FALSE)
}

effective_coefficients <- function(model, theta, variables, group_of) {
  n <- length(group_of)
  design <- model$design
  p <- sum(design$n_levels - 1L)
  beta <- matrix(0, n, p)
  offsets <- cumsum(c(0L, design$n_levels[-length(design$n_levels)] - 1L))
  for (nm in names(model$terms)) {
    term <- model$terms[[nm]]
    k <- match(term$attribute, names(design$levels))
    columns <- offsets[k] + seq_len(design$n_levels[k] - 1L)
    coefficients <- which(model$coefficient_metadata$term == nm)
    if (term$type == "dummy") beta[, columns] <- beta[, columns, drop = FALSE] + theta[, coefficients, drop = FALSE]
    else {
      scores <- latent_term_scores(model, term, variables, group_of)
      gaps <- scores[, -1L, drop = FALSE] - scores[, 1L]
      beta[, columns] <- beta[, columns, drop = FALSE] + gaps * theta[, coefficients]
    }
  }
  beta
}

draw_latent_respondents <- function(model, group_of) {
  n <- length(group_of)
  variables <- list(matches = list(), ratings = list())
  for (nm in names(model$terms)) {
    term <- model$terms[[nm]]
    if (term$type != "match") next
    levels <- length(model$design$levels[[term$attribute]])
    matches <- rep(NA_integer_, n)
    for (g in seq_len(nrow(model$coef_mean))) {
      rows <- which(group_of == g)
      values <- sample.int(levels + 1L, length(rows), replace = TRUE, prob = term$probabilities[g, ])
      values[values == levels + 1L] <- NA_integer_
      matches[rows] <- values
    }
    variables$matches[[nm]] <- matches
  }
  for (nm in names(model$terms)) {
    term <- model$terms[[nm]]
    if (term$type != "rating") next
    levels <- model$design$levels[[term$attribute]]
    deviations <- matrix(stats::rnorm(n * length(levels)), nrow = n)
    ratings <- term$mean[group_of, , drop = FALSE] + deviations * term$sd[group_of, , drop = FALSE]
    dimnames(ratings) <- list(NULL, levels)
    variables$ratings[[nm]] <- ratings
  }
  deviations <- matrix(stats::rnorm(n * ncol(model$coef_mean)), nrow = n)
  theta <- model$coef_mean[group_of, , drop = FALSE] + deviations * model$coef_sd[group_of, , drop = FALSE]
  list(variables = variables, beta = effective_coefficients(model, theta, variables, group_of))
}

latent_score_columns <- function(data, model, variables) {
  group <- if (is.null(model$groups)) rep(1L, nrow(data)) else match(data$group, model$groups)
  for (nm in names(model$terms)) {
    term <- model$terms[[nm]]
    if (term$type == "dummy") next
    level <- as.integer(data[[term$attribute]])
    if (term$type == "score") data[[nm]] <- term$scores[cbind(group, level)]
    else if (term$type == "rating") {
      data[[nm]] <- variables$ratings[[nm]][cbind(data$respondent, level)]
    } else {
      match <- variables$matches[[nm]][data$respondent]
      data[[nm]] <- as.numeric(!is.na(match) & match == level)
    }
  }
  data
}

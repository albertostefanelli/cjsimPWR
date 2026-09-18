#' Estimate AMCEs and differences between subgroup AMCEs
#'
#' @param data Profile rows with finite numeric `y`, a non-missing `respondent` identifier, the
#'   design's attribute columns, and `group` when estimating subgroup effects. A respondent must
#'   belong to only one group. Missing outcomes or attribute values are rejected, never omitted.
#' @param design A `cj_design` from `conjoint_design()`. Its first level is always the reference,
#'   even when that level is absent from the sample.
#' @param groups Group names in the desired order. Each subsequent group is compared with the first.
#'   If NULL, use the group factor's levels, or first appearance for character groups; an absent or
#'   entirely NA group column means no subgroups. Supply this argument to fix character-group order
#'   independently of row order. A single group produces AMCEs and no differences.
#' @param vcov `"CR1"` (default) or `"CR2"`. CR1 is computed with
#'   `sandwich::vcovCL(type = "HC1", cadjust = TRUE)`: the cluster-robust covariance times
#'   G/(G-1) * (n-1)/(n-k), where G is all respondents in the joint fit, n is profile rows and k is the
#'   fitted rank (`clubSandwich`'s `"CR1S"`, and the estimator of `cjoint::amce()` and Stata's
#'   `vce(cluster)`). CR2 is the bias-reduced estimator of Bell and McCaffrey (2002) and requires the
#'   `clubSandwich` package.
#' @param inference `"normal"`, `"t"` (G-1 df from the joint fit), or `"Satterthwaite"` (CR2 only).
#'   NULL chooses normal for CR1 and Satterthwaite for CR2. The G-1 approximation can be poor for
#'   small subgroups, even if the joint fit has many respondents.
#' @param alpha Significance level for two-sided confidence intervals and tests of zero.
#'
#' @return A data frame in supplied group, attribute and level order. AMCE rows precede differences.
#'   `group` is a display label; `group_name` and `reference_group` identify the contrast without
#'   parsing labels. Other columns include `type`, `attribute`, `level`, `reference_level`,
#'   `estimate`, `std.error`, `df`, `statistic`, `p.value`, `conf.low`, `conf.high`, `vcov`,
#'   `inference`, `n_clusters`, `n_clusters_effect`, `rank`, `status` and `failure_reason`.
#'   Non-estimable effects retain a row with NA estimates/inference; estimable effects can retain
#'   their estimates when inference fails. Each group involved in an effect needs at least two
#'   respondents for inference. Covariance is attached as attribute `vcov`, in output row order;
#'   rows/columns without valid inference are NA. Perfectly fitted subgroups have `invalid_variance`
#'   for their AMCEs; a difference can retain inference when another group supplies positive variance.
#'   No multiplicity adjustment is applied.
#' @export
#' @md
#' @examples
#' design <- conjoint_design(c(2, 3))
#' data <- simulate_experiment(design, list(0.05, c(-0.05, 0.1)), units = 100, n_tasks = 3)
#' estimate_amce(data, design)
#' estimate_amce(data, design, inference = "t")
#'
#' # subgroup AMCEs and their differences from the first group
#' amce_by_group <- list(A = list(0.05, c(-0.05, 0.1)), B = list(0.1, c(0, 0.1)))
#' grouped <- simulate_experiment(design, amce_by_group, units = c(A = 100, B = 80), n_tasks = 3,
#'                                groups = c("A", "B"))
#' estimate_amce(grouped, design, groups = c("A", "B"))
estimate_amce <- function(data, design, groups = NULL, vcov = c("CR1", "CR2"),
                          inference = NULL, alpha = 0.05) {
  vcov <- match.arg(vcov)
  inference <- check_inference(vcov, inference, alpha)
  input <- amce_model_data(data, design, groups)
  X <- input$X
  C <- input$C
  out <- input$table
  n <- nrow(X)
  G <- length(unique(data$respondent))
  fit <- stats::lm.fit(X, data$y)
  k <- fit$rank
  keep <- fit$qr$pivot[seq_len(k)]
  Xr <- X[, keep, drop = FALSE]
  reduced <- stats::lm.fit(Xr, data$y)
  Cr <- C[, keep, drop = FALSE]

  # X = Xr A. A requested functional C beta is estimable exactly when C = Cr A.
  # This retains identified effects without treating arbitrary aliased coefficients as zero.
  estimable <- rep(TRUE, nrow(C))
  if (k < ncol(X)) {
    A <- qr.coef(reduced$qr, X)
    estimable <- apply(abs(C - Cr %*% A), 1L, max) < 1e-8
  }
  out$estimate <- as.numeric(Cr %*% reduced$coefficients)
  out$estimate[!estimable] <- NA_real_
  for (nm in c("std.error", "df", "statistic", "p.value", "conf.low", "conf.high")) {
    out[[nm]] <- NA_real_
  }
  out$vcov <- vcov
  out$inference <- inference
  out$n_clusters <- G
  out$rank <- k
  out$failure_reason <- NA_character_
  out$failure_reason[!estimable] <- "non_estimable"
  out$failure_reason[estimable & input$min_clusters < 2] <- "insufficient_clusters"
  if (n <= k) out$failure_reason[is.na(out$failure_reason)] <- "no_residual_df"
  eligible <- which(is.na(out$failure_reason))
  Vout <- matrix(NA_real_, nrow(C), nrow(C))

  if (length(eligible)) {
    # Safe internal column names avoid all formula/label parsing. A genuine lm object on the full-rank
    # reduced design supplies what sandwich and clubSandwich need; its coefficients are in Xr order.
    model_data <- data.frame(y = data$y, Xr, check.names = FALSE)
    if (vcov == "CR1") {
      # G/(G - 1) from cadjust and (n - 1)/(n - k) from HC1: the CR1S estimator, as in cjoint and Stata.
      cov_result <- tryCatch({
        model <- stats::lm(y ~ . - 1, data = model_data)
        # An exact fit makes summary.lm() warn; it is classified as invalid_variance below.
        V <- withCallingHandlers(
          sandwich::vcovCL(model, cluster = data$respondent, type = "HC1", cadjust = TRUE),
          warning = function(w) {
            if (grepl("essentially perfect fit", conditionMessage(w))) invokeRestart("muffleWarning")
          })
        list(V = unname(V), df = rep(if (inference == "t") G - 1 else Inf, length(eligible)))
      }, error = function(e) list(error = conditionMessage(e)))
    } else {
      cov_result <- tryCatch({
        model <- stats::lm(y ~ . - 1, data = model_data, x = TRUE, y = TRUE)
        V <- clubSandwich::vcovCR(model, cluster = data$respondent, type = "CR2")
        tests <- clubSandwich::linear_contrast(model, vcov = V,
          contrasts = Cr[eligible, , drop = FALSE], level = 1 - alpha,
          test = switch(inference, normal = "z", t = "naive-t", Satterthwaite = "Satterthwaite"))
        list(V = V, df = tests$df)
      }, error = function(e) list(error = conditionMessage(e)))
    }
    if (!is.null(cov_result$error)) {
      out$failure_reason[eligible] <- "covariance_failure"
      attr(out, "covariance_error") <- cov_result$error
    } else {
      V <- cov_result$V
      # A perfectly fitted subgroup can leave roundoff covariance even when other groups are noisy.
      # Zero its coefficient block before forming contrasts; unaffected groups retain their variance.
      for (g in unique(input$group_index)) {
        rows <- input$group_index == g
        if (sum(reduced$residuals[rows]^2) <=
            (100 * .Machine$double.eps)^2 * max(1, sum(data$y[rows]^2))) {
          columns <- input$coefficient_group[keep] == g
          V[columns, ] <- 0
          V[, columns] <- 0
        }
      }
      Vout <- Cr %*% V %*% t(Cr)
      variance <- diag(Vout)[eligible]
      good_var <- is.finite(variance) & variance > 0
      good_df <- !is.na(cov_result$df) & cov_result$df > 0
      out$failure_reason[eligible[!good_var]] <- "invalid_variance"
      out$failure_reason[eligible[good_var & !good_df]] <- "invalid_df"
      good <- eligible[good_var & good_df]
      out$std.error[good] <- sqrt(diag(Vout)[good])
      out$df[good] <- cov_result$df[good_var & good_df]
      out$statistic[good] <- out$estimate[good] / out$std.error[good]
      out$p.value[good] <- 2 * stats::pt(abs(out$statistic[good]), df = out$df[good], lower.tail = FALSE)
      critical <- stats::qt(1 - alpha / 2, df = out$df[good])
      out$conf.low[good] <- out$estimate[good] - critical * out$std.error[good]
      out$conf.high[good] <- out$estimate[good] + critical * out$std.error[good]
    }
  }
  out$status <- ifelse(is.na(out$failure_reason), "ok", "failed")
  failed <- out$status == "failed"
  Vout[failed, ] <- NA_real_
  Vout[, failed] <- NA_real_
  attr(out, "vcov") <- Vout
  out
}

# Shared preflight: configuration errors must stop power_sim(), not become failed experiments.
check_inference <- function(vcov, inference, alpha) {
  if (is.null(inference)) inference <- if (vcov == "CR2") "Satterthwaite" else "normal"
  inference <- match.arg(inference, c("normal", "t", "Satterthwaite"))
  if (inference == "Satterthwaite" && vcov != "CR2") {
    stop("Satterthwaite inference requires `vcov = \"CR2\"`.", call. = FALSE)
  }
  if (vcov == "CR2" && !requireNamespace("clubSandwich", quietly = TRUE)) {
    stop("`vcov = \"CR2\"` requires the suggested package 'clubSandwich'.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  inference
}

# Validate inputs and construct design columns, metadata and contrast rows together. User labels
# are values only; never formula fragments, coefficient names or delimited identifiers.
amce_model_data <- function(data, design, groups) {
  if (!inherits(design, "cj_design")) {
    stop("`design` must be created with conjoint_design().", call. = FALSE)
  }
  attrs <- names(design$levels)
  if (!is.data.frame(data) || nrow(data) == 0 || anyDuplicated(names(data)) ||
      !all(c("y", "respondent", attrs) %in% names(data))) {
    stop("`data` must have rows and unique columns including y, respondent and every design attribute.", call. = FALSE)
  }
  if (!is.numeric(data$y) || !is.null(dim(data$y)) || any(!is.finite(data$y))) {
    stop("`y` must be finite numeric values with no missing observations.", call. = FALSE)
  }
  id <- data$respondent
  if (!is.atomic(id) || !is.null(dim(id)) || anyNA(id) || any(as.character(id) == "") ||
      (is.numeric(id) && any(!is.finite(id)))) {
    stop("`respondent` must contain finite, non-missing identifiers.", call. = FALSE)
  }
  groups <- check_groups(groups)
  grouped <- !is.null(groups) || ("group" %in% names(data) && !all(is.na(data$group)))
  if (grouped) {
    if (!"group" %in% names(data) || anyNA(data$group)) {
      stop("`group` must be present and non-missing for subgroup estimation.", call. = FALSE)
    }
    if (is.null(groups)) {
      groups <- check_groups(if (is.factor(data$group)) levels(data$group) else unique(as.character(data$group)))
    }
    group_index <- match(as.character(data$group), groups)
    if (anyNA(group_index)) stop("data group names must match `groups`.", call. = FALSE)
    if (any(group_index != group_index[match(id, id)])) {
      stop("each `respondent` must belong to one group; identifiers must be unique across groups.", call. = FALSE)
    }
  } else {
    groups <- NA_character_
    group_index <- rep.int(1L, nrow(data))
  }
  meta <- do.call(rbind, lapply(attrs, function(a) {
    lv <- design$levels[[a]]
    data.frame(attribute = a, level = lv[-1], reference_level = lv[1], stringsAsFactors = FALSE)
  }))
  D <- do.call(cbind, lapply(attrs, function(a) {
    values <- as.character(data[[a]])
    lv <- design$levels[[a]]
    if (anyNA(values) || any(!values %in% lv)) {
      stop("attribute '", a, "' contains missing values or levels outside the design.", call. = FALSE)
    }
    vapply(lv[-1], function(l) as.numeric(values == l), numeric(nrow(data)))
  }))
  B <- cbind(1, D)
  p <- nrow(meta)
  ng <- length(groups)
  X <- do.call(cbind, lapply(seq_len(ng), function(g) B * (group_index == g)))
  colnames(X) <- paste0("b", seq_len(ncol(X)))
  clusters <- vapply(seq_len(ng), function(g) length(unique(id[group_index == g])), integer(1))
  table <- do.call(rbind, lapply(seq_len(ng), function(g) {
    data.frame(type = "amce", group = groups[g], group_name = groups[g], reference_group = NA_character_,
               meta, n_clusters_effect = clusters[g], stringsAsFactors = FALSE)
  }))
  C <- matrix(0, ng * p, ncol(X))
  for (g in seq_len(ng)) C[cbind((g - 1L) * p + seq_len(p), (g - 1L) * (p + 1L) + 1L + seq_len(p))] <- 1
  min_clusters <- rep(clusters, each = p)
  if (ng > 1) {
    diff_rows <- do.call(rbind, lapply(2:ng, function(g) {
      data.frame(type = "difference", group = paste(groups[g], "-", groups[1]), group_name = groups[g],
                 reference_group = groups[1], meta, n_clusters_effect = clusters[g] + clusters[1],
                 stringsAsFactors = FALSE)
    }))
    differences <- do.call(rbind, lapply(2:ng, function(g) {
      C[(g - 1L) * p + seq_len(p), , drop = FALSE] - C[seq_len(p), , drop = FALSE]
    }))
    C <- rbind(C, differences)
    table <- rbind(table, diff_rows)
    min_clusters <- c(min_clusters, rep(pmin(clusters[-1], clusters[1]), each = p))
  }
  rownames(table) <- NULL
  list(X = X, C = C, table = table, min_clusters = min_clusters,
       group_index = group_index, coefficient_group = rep(seq_len(ng), each = p + 1L))
}

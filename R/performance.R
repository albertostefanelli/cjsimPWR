#' Performance of an AMCE estimate across simulation runs
#'
#' @description Summarises the estimates of one AMCE from repeated simulations: power (or the Type I error
#'   rate for a null target), Type S and Type M errors, coverage, bias and standard errors, each
#'   with its Monte Carlo standard error.
#'
#' @param estimate numeric vector; the estimated AMCE in each run.
#' @param std.error numeric vector; the standard error of each estimate.
#' @param truth numeric; the generated population AMCE, or its independent numerical reference estimate.
#' @param alpha numeric; significance level of the two-sided test. Default 0.05.
#' @param df numeric; degrees of freedom for t critical values, either one value or one per run. `Inf`
#'   (the default) uses normal critical values. `NA` marks unavailable inference in a failed run.
#' @param null logical; classify the target as a null? Defaults to `truth == 0`. Set TRUE for a
#'   requested null whose generated contrast only approximately equals zero. Its rejection rate is
#'   then an approximate Type I error rate; bias and coverage still use `truth`.
#' @param reference_mcse non-negative number; Monte Carlo SE of the independent reference estimate
#'   of `truth`, default 0. Its variance is added to `bias_mcse` squared, without division by run count.
#' @param reference_half_width non-negative number; half-width of the reference interval for `truth`,
#'   default 0. Used for coverage sensitivity bounds; see Details.
#'
#' @details A run is significant when `|estimate / std.error|` exceeds the critical value. Runs with a
#'   missing or infinite estimate, a missing, infinite or non-positive standard error, or unavailable
#'   per-run degrees of freedom are counted in `n_failed` and left out of every measure. Measures are
#'   conditional on the `n_valid` runs with valid inference; `n_runs` counts all attempted runs.
#'
#' @return A one-row data frame of class `cj_performance` with:
#'
#'   **Rejection**, denominator `n_valid`:
#'   * `power` (when `null` is FALSE) or `type_1_error` (when TRUE): share of significant runs. Exactly
#'     one of the two is non-`NA`. `power_mcse`/`type_1_error_mcse`: binomial Monte Carlo SE,
#'     `sqrt(p * (1 - p) / n_valid)`.
#'
#'   **Sign and magnitude** (Gelman and Carlin 2014), denominator `n_sig`, the significant runs among
#'   `n_valid`; `NA` when `null` is TRUE, `truth` is zero, or `n_sig` is zero:
#'   * `type_s`: share of significant runs whose estimate has the opposite sign to `truth`. `type_s_mcse`:
#'     the same binomial formula, on `n_sig`.
#'   * `type_m`: exaggeration ratio, the mean of `|estimate| / |truth|` over significant runs.
#'     `type_m_mcse`: `sd(ratio) / sqrt(n_sig)`, `NA` when `n_sig < 2`.
#'
#'   **Bias and standard errors**, denominator `n_valid` (`n` below):
#'   * `mean_estimate`: `mean(estimate)` over valid runs.
#'   * `bias`: `mean_estimate - truth`.
#'   * `emp_se`: `sd(estimate)` over valid runs; `NA` when `n < 2`.
#'   * `model_se`: the root mean square of the reported standard errors, `sqrt(mean(std.error^2))`,
#'     over valid runs.
#'   * `bias_mcse = sqrt(emp_se^2 / n + reference_mcse^2)`. The reference error is shared across
#'     experiments and does not disappear with more runs; this formula assumes an independent reference.
#'   * `emp_se_mcse`: the standard large-sample approximation `emp_se / sqrt(2 * (n - 1))`, `NA` when
#'     `n < 2`.
#'   * `model_se_mcse`: the analogous approximation for the root-mean-square SE,
#'     `sqrt(var(std.error^2) / (4 * n * model_se^2))`, `NA` when `n < 2`.
#'
#'   **Coverage and reference sensitivity**, denominator `n_valid`, remaining conditional on the
#'   supplied reference:
#'   * `coverage`: share of runs whose confidence interval, `estimate` plus or minus the critical value
#'     times `std.error`, contains `truth`. `coverage_mcse`: the binomial formula on `n_valid`.
#'   * `coverage_reference_lower`, `coverage_reference_upper`: bounds on coverage over candidate
#'     references within `reference_half_width` of `truth` — not confidence intervals for coverage.
#'     `_lower` counts intervals containing the entire reference interval; `_upper` counts intervals
#'     intersecting it. Increase reference precision if the two differ materially.
#'
#'   **Analytic diagnostics** (Lu, Qiu and Deng 2019), `NA` for null targets: `analytic_power`,
#'   `analytic_type_s`, `analytic_type_m`. A known-SE normal approximation using `truth` and the
#'   *empirical* SD `emp_se` — not `model_se` — even when significance above used t inference. This is
#'   a rough check at low power, and is not the calculation used by cjpowR or other closed-form tools;
#'   see the [closed-form comparison](https://github.com/albertostefanelli/cjsimPWR/blob/main/docs/closed_form.md)
#'   (online).
#'
#'   **Counts:** `n_sig`, `n_runs` (all attempted runs), `n_valid`, `n_failed`.
#'
#'   The default printout includes Type I error only for null targets, never because an estimate is
#'   nonsignificant; for approximate nulls this is rejection under the generated, near-zero contrast.
#'   Monte Carlo standard errors (`_mcse` columns) follow Morris, White and Crowther (2019).
#'   `power_sim()` also checks approximate nulls for sensitivity to calibration error; see its help
#'   for the warning threshold and assumptions. A standalone `summarise_runs()` call emits no such
#'   warning, since it constructs no target error bound.
#'
#' @references
#' Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing Type S (sign) and Type M
#' (magnitude) errors. *Perspectives on Psychological Science*, 9(6), 641-651. \doi{10.1177/1745691614551642}
#'
#' Lu, J., Qiu, Y., & Deng, A. (2019). A note on Type S/M errors in hypothesis testing. *British Journal of
#' Mathematical and Statistical Psychology*, 72(1), 1-17. \doi{10.1111/bmsp.12132}
#'
#' Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation studies to evaluate statistical
#' methods. *Statistics in Medicine*, 38(11), 2074-2102. \doi{10.1002/sim.8086}
#'
#' @seealso [power_sim()], [estimate_amce()]
#' @export
#' @md
#'
#' @examples
#' # 1,000 estimates of an AMCE of 0.05 with a standard error of 0.05
#' set.seed(1)
#' estimates <- rnorm(1000, mean = 0.05, sd = 0.05)
#' summarise_runs(estimates, std.error = rep(0.05, 1000), truth = 0.05)
#'
#' # Under a true null, the rejection rate is Type I error rather than power
#' set.seed(1)
#' null_estimates <- rnorm(1000, mean = 0, sd = 0.05)
#' summarise_runs(null_estimates, std.error = rep(0.05, 1000), truth = 0)
#'
#' # An approximate (calibrated) null: truth is illustrative, an independent reference near
#' # but not exactly zero, as power_sim() would supply from a calibrated model's verification
#' set.seed(1)
#' approx_estimates <- rnorm(1000, mean = 0.0003, sd = 0.05)
#' summarise_runs(approx_estimates, std.error = rep(0.05, 1000), truth = 0.0003, null = TRUE,
#'                reference_mcse = 0.0001, reference_half_width = 0.0003)

summarise_runs <- function(estimate, std.error, truth, alpha = 0.05, df = Inf,
                           null = truth == 0, reference_mcse = 0, reference_half_width = 0) {
  if (!is.numeric(estimate)) {
    stop("`estimate` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(std.error) || length(std.error) != length(estimate)) {
    stop("`std.error` must be numeric with one value per estimate.", call. = FALSE)
  }
  if (!is.numeric(truth) || length(truth) != 1 || !is.finite(truth)) {
    stop("`truth` must be a single finite number.", call. = FALSE)
  }
  if (!is.logical(null) || length(null) != 1 || is.na(null)) {
    stop("`null` must be TRUE or FALSE.", call. = FALSE)
  }
  for (nm in c("reference_mcse", "reference_half_width")) {
    value <- get(nm)
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 0) {
      stop("`", nm, "` must be a single non-negative finite number.", call. = FALSE)
    }
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !isTRUE(alpha > 0 && alpha < 1)) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(df) || !length(df) %in% c(1, length(estimate)) ||
      any(df <= 0, na.rm = TRUE)) {
    stop("`df` must be positive (or NA for failed inference), with one value or one value per estimate.", call. = FALSE)
  }

  n_runs <- length(estimate)
  df <- rep_len(df, n_runs)
  ok <- is.finite(estimate) & is.finite(std.error) & std.error > 0 & !is.na(df)
  est <- estimate[ok]
  se <- std.error[ok]
  df <- df[ok]
  n <- length(est)
  crit <- ifelse(is.infinite(df), stats::qnorm(1 - alpha / 2), stats::qt(1 - alpha / 2, df))

  sig <- abs(est / se) > crit
  n_sig <- sum(sig)
  rejection <- proportion(sig)
  undefined_sm <- null || truth == 0
  ratio <- if (undefined_sm) numeric() else abs(est[sig]) / abs(truth)
  type_s <- if (undefined_sm || n_sig == 0) NA_real_ else mean(sign(est[sig]) != sign(truth))
  type_m <- if (undefined_sm || n_sig == 0) NA_real_ else mean(ratio)
  coverage <- proportion(abs(est - truth) <= crit * se)

  emp_se <- if (n > 1) stats::sd(est) else NA_real_
  model_se <- if (n > 0) sqrt(mean(se^2)) else NA_real_
  analytic <- closed_form_measures(truth, emp_se, alpha)

  out <- data.frame(
    power = if (null) NA_real_ else rejection,
    power_mcse = if (null) NA_real_ else proportion_mcse(rejection, n),
    type_1_error = if (null) rejection else NA_real_,
    type_1_error_mcse = if (null) proportion_mcse(rejection, n) else NA_real_,
    type_s = type_s,
    type_s_mcse = proportion_mcse(type_s, n_sig),
    type_m = type_m,
    type_m_mcse = if (is.na(type_m) || n_sig < 2) NA_real_ else stats::sd(ratio) / sqrt(n_sig),
    coverage = coverage,
    coverage_mcse = proportion_mcse(coverage, n),
    coverage_reference_lower = proportion(abs(est - truth) + reference_half_width <= crit * se),
    coverage_reference_upper = proportion(pmax(0, abs(est - truth) - reference_half_width) <= crit * se),
    mean_estimate = if (n > 0) mean(est) else NA_real_,
    bias = if (n > 0) mean(est) - truth else NA_real_,
    bias_mcse = sqrt(emp_se^2 / n + reference_mcse^2),
    emp_se = emp_se,
    emp_se_mcse = if (n > 1) emp_se / sqrt(2 * (n - 1)) else NA_real_,
    model_se = model_se,
    model_se_mcse = if (n > 1) sqrt(stats::var(se^2) / (4 * n * model_se^2)) else NA_real_,
    analytic_power = if (null) NA_real_ else analytic$power,
    analytic_type_s = if (undefined_sm) NA_real_ else analytic$type_s,
    analytic_type_m = if (undefined_sm) NA_real_ else analytic$type_m,
    n_sig = n_sig,
    n_runs = n_runs,
    n_valid = n,
    n_failed = n_runs - n
  )
  class(out) <- c("cj_performance", class(out))
  out
}

# Share of TRUE values; NA when there are none.
proportion <- function(x) {
  if (length(x) == 0) NA_real_ else mean(x)
}

# Monte Carlo standard error of a proportion p estimated from n runs.
proportion_mcse <- function(p, n) {
  if (is.na(p) || n == 0) NA_real_ else sqrt(p * (1 - p) / n)
}

#' @export
print.cj_performance <- function(x, digits = 3, ...) {
  labels <- c(power = "Power", type_1_error = "Type I error", type_s = "Type S", type_m = "Type M",
              coverage = "Coverage")
  out <- data.frame(row.names = seq_len(nrow(x)))
  for (col in intersect(c("type", "group", "attribute", "level", "true_amce"), names(x))) {
    out[[col]] <- if (col == "true_amce") round(x[[col]], 4) else x[[col]]
  }
  for (m in names(labels)) {
    if (m %in% names(x) && !all(is.na(x[[m]]))) {
      out[[labels[[m]]]] <- format_with_mcse(x[[m]], x[[paste0(m, "_mcse")]], digits)
    }
  }
  if (all(c("n_runs", "n_failed") %in% names(x))) {
    out[["Runs (failed)"]] <- paste0(x$n_runs, " (", x$n_failed, ")")
  }
  if ("n_valid" %in% names(x)) out[["Valid runs"]] <- x$n_valid
  print(out, row.names = FALSE, right = FALSE)
  cat("Monte Carlo standard errors in parentheses.\n")
  if ("Type I error" %in% names(out)) {
    cat("Power is for non-null targets; Type I error is for null targets, not nonsignificant estimates.\n")
  }
  cat("Measures are conditional on runs with valid inference.\n")
  invisible(x)
}

# Columns a cj_performance table needs for its print method; subsets without them are plain data frames.
performance_columns <- c("power", "power_mcse", "type_1_error", "type_1_error_mcse", "type_s", "type_s_mcse",
                         "type_m", "type_m_mcse", "coverage", "coverage_mcse", "n_runs", "n_failed")

#' @export
`[.cj_performance` <- function(x, ...) {
  out <- NextMethod()
  if (is.data.frame(out) && !all(performance_columns %in% names(out))) {
    class(out) <- setdiff(class(out), "cj_performance")
  }
  out
}

# "estimate (mcse)", or just the estimate when its Monte Carlo SE is missing; empty when both are missing.
format_with_mcse <- function(value, mcse, digits) {
  if (is.null(mcse)) mcse <- rep(NA_real_, length(value))
  shown <- formatC(value, digits = digits, format = "f")
  with_mcse <- paste0(shown, " (", formatC(mcse, digits = digits, format = "f"), ")")
  ifelse(is.na(value), "", ifelse(is.na(mcse), shown, with_mcse))
}

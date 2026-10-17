# Latent-model orchestration. Truth calculation precedes the experiment streams and each
# experiment is generated once, irrespective of the requested analyses.
power_latent <- function(model, units, n_tasks, estimators, paired_terms, scale,
                          vcov, inference, alpha, sim_runs, seed, cores, keep_runs) {
  analysis <- NULL
  if ("paired" %in% estimators) {
    if (is.null(paired_terms)) paired_terms <- names(model$terms)
    analysis <- paired_analysis(model$design, paired_terms, model)
    population_gram(model, analysis)
  }
  needs_amce <- "amce" %in% estimators || identical(scale, "probability")
  if (needs_amce && !any(model$truth$estimator == "amce")) {
    message("Computing AMCE population targets; reuse result$model to retain this cache.")
    added <- latent_truth(model, analysis = NULL, amce = TRUE)
    model$truth <- rbind(model$truth, added$truth)
    # Keep the cached paired calculation exactly as originally prepared. AMCE diagnostics
    # have their own precision family, so adding them cannot widen a paired interval.
    model$diagnostics$amce <- added$diagnostics
    model$diagnostics$families$amce <- added$diagnostics$families$amce
  }
  diagnostics <- model$diagnostics
  latent_reference <- NULL
  if ("paired" %in% estimators && identical(scale, "latent")) {
    if (identical(paired_terms, names(model$terms))) {
      latent_reference <- model$truth[model$truth$estimator == "paired" & model$truth$scale == "latent", , drop = FALSE]
    } else {
      # Length-prefixed UTF-8 names preserve order without collisions from delimiters in labels.
      terms_utf8 <- enc2utf8(paired_terms)
      key <- paste0(nchar(terms_utf8, type = "bytes"), ":", terms_utf8, collapse = "")
      reference <- model$analysis_truths[[key]]
      if (is.null(reference)) {
        reference <- latent_truth(model, analysis, amce = FALSE)
        model$analysis_truths[[key]] <- reference
      }
      latent_reference <- reference$truth
      diagnostics$paired_analysis <- reference$diagnostics
    }
  }
  truth <- do.call(rbind, lapply(estimators, function(estimator) {
    if (estimator == "paired" && identical(scale, "latent")) return(latent_reference)
    out <- model$truth[model$truth$estimator == "amce", , drop = FALSE]
    out$estimator <- estimator
    out
  }))
  rownames(truth) <- NULL
  truth <- data.frame(effect_id = seq_len(nrow(truth)), truth, row.names = NULL)
  truth$in_analysis <- TRUE
  if (!is.null(analysis) && identical(scale, "probability")) {
    omitted <- setdiff(names(model$design$levels), unique(analysis$attribute))
    if (length(omitted)) {
      truth$in_analysis[truth$estimator == "paired" & truth$attribute %in% omitted] <- FALSE
      message("Paired probability analysis omits: ", paste(omitted, collapse = ", "),
        ". Its predictions do not use these attributes; their rows retain the full AMCE targets ",
        "and are marked in_analysis = FALSE.")
    }
  }
  unresolved <- !truth$null & !is.na(truth$sign_resolved) & !truth$sign_resolved
  if (any(unresolved)) {
    warning(latent_sign_warning(truth[unresolved, , drop = FALSE]), call. = FALSE)
  }
  worker <- latent_power_worker(model, units, n_tasks, estimators, paired_terms, scale,
                               vcov, inference, alpha, truth)
  workers <- min(cores, sim_runs)
  results <- with_reference_seed(seed, {
    streams <- vector("list", sim_runs)
    streams[[1L]] <- get(".Random.seed", envir = .GlobalEnv)
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
  # Index each effect's runs once instead of rescanning the run table per effect.
  by_effect <- split(seq_len(nrow(runs)), factor(runs$effect_id, levels = truth$effect_id))
  performance <- do.call(rbind, lapply(truth$effect_id, function(id) {
    i <- by_effect[[id]]
    summarise_runs(runs$estimate[i], runs$std.error[i], truth$true_value[id], alpha, runs$df[i],
      null = truth$null[id], reference_mcse = truth$reference_mcse[id],
      reference_half_width = truth$reference_half_width[id])
  }))
  performance <- data.frame(truth, vcov = vcov, inference = inference, performance, row.names = NULL)
  class(performance) <- c("cj_performance", "data.frame")
  labels <- c("effect_id", "estimator", "scale", "term", "type", "group", "group_name",
              "reference_group", "attribute", "level", "reference_level")
  failures <- power_failures(runs, truth, labels)
  settings <- list(design = model$design, units = units, n_tasks = n_tasks, groups = model$groups,
    dgp = model$dgp, specification = model$specification, estimators = estimators,
    paired_terms = paired_terms, scale = if (is.null(scale)) "probability" else scale,
    alpha = alpha, vcov = vcov, inference = inference, sim_runs = sim_runs, seed = seed,
    cores = cores, workers = workers, keep_runs = keep_runs, control = model$control,
    rng = c("L'Ecuyer-CMRG", "Inversion", "Rejection"))
  if (!is.null(model$groups)) names(settings$units) <- names(settings$n_tasks) <- model$groups
  structure(list(performance = performance, truth = truth, parameters = model$parameters,
    diagnostics = diagnostics, settings = settings, failures = failures,
    runs = if (keep_runs) runs else NULL, model = model), class = "cj_power")
}

latent_sign_warning <- function(rows) {
  details <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    effect <- if (!is.na(row$level)) paste0(row$attribute, " / ", row$level) else row$term
    label <- paste(c(row$estimator, row$scale, row$group[!is.na(row$group)], effect), collapse = "; ")
    if (row$reference_half_width == 0 && row$solver_error == 0) {
      advice <- "within floating-point roundoff of zero; a larger integration budget cannot resolve this roundoff"
    } else if (row$reference_half_width >= row$solver_error + row$roundoff_guard) {
      controls <- if (row$scale == "latent") "paired_cells" else "verification_draws / verification_pairs"
      advice <- paste0("within its integration uncertainty of zero. If it is not zero by design, ",
                        "increase the integration budget (", controls, " in utility_model(control = ...))")
    } else if (row$scale == "probability") {
      advice <- "within floating-point roundoff of zero; more reference simulation alone cannot resolve this roundoff"
    } else {
      advice <- "within its numerical uncertainty of zero; inspect solver/scaling diagnostics"
    }
    paste0(label, ": ", advice, ".")
  }, character(1))
  paste0("Unresolved truth sign:\n", paste(details, collapse = "\n"),
    "\nIf an effect is zero by design, its reported power is a Type I error rate and its Type S/M ",
    "are not meaningful, although they are still reported.")
}

# Collect only package helpers reachable from the sampling/estimation entry points. all.names()
# also finds functions passed as values (for example lapply(..., helper)), unlike a call-only scan.
# Every copied function resolves into the same bounded base-parent environment on PSOCK workers.
latent_worker_environment <- function() {
  source <- environment(latent_worker_environment)
  env <- new.env(parent = baseenv())
  pending <- c("simulate_experiment", "estimate_amce", "estimate_paired", "match_effect_rows")
  copied <- character()
  while (length(pending)) {
    nm <- pending[1L]
    pending <- pending[-1L]
    if (nm %in% copied || !exists(nm, envir = source, inherits = FALSE)) next
    fn <- get(nm, envir = source, inherits = FALSE)
    if (!is.function(fn)) next
    dependencies <- unique(all.names(body(fn), functions = TRUE))
    # Include symbols as well as calls so callbacks are copied too.
    dependencies <- union(dependencies, all.names(body(fn), functions = FALSE))
    environment(fn) <- env
    env[[nm]] <- fn
    copied <- c(copied, nm)
    pending <- union(pending, setdiff(dependencies, copied))
  }
  env
}

latent_power_worker <- function(model, units, n_tasks, estimators, paired_terms, scale,
                                vcov, inference, alpha, truth) {
  env <- latent_worker_environment()
  labels <- c("effect_id", "estimator", "scale", "term", "type", "group", "group_name",
              "reference_group", "attribute", "level", "reference_level")
  list2env(list(model = model, units = units, n_tasks = n_tasks, estimators = estimators,
    paired_terms = paired_terms, scale = scale, vcov = vcov, inference = inference,
    alpha = alpha, template = truth[labels]), envir = env)
  eval(quote(function(stream) {
    assign(".Random.seed", stream, envir = .GlobalEnv)
    blank <- function(template, message) {
      for (nm in c("n_clusters_effect", "estimate", "std.error", "df", "statistic", "p.value",
                   "conf.low", "conf.high", "n_clusters", "rank")) template[[nm]] <- NA_real_
      template$vcov <- vcov
      template$inference <- inference
      template$failure_reason <- "run_error"
      template$status <- "failed"
      template$error_message <- message
      template
    }
    sampled <- tryCatch(list(data = simulate_experiment(model$design, units = units,
      n_tasks = n_tasks, model = model)), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(sampled$error)) return(blank(template, sampled$error))
    do.call(rbind, lapply(estimators, function(estimator) {
      target <- template[template$estimator == estimator, , drop = FALSE]
      tryCatch({
        fit <- if (estimator == "amce") {
          estimate_amce(sampled$data, model$design, groups = model$groups,
                        vcov = vcov, inference = inference, alpha = alpha)
        } else {
          estimate_paired(sampled$data, model$design, terms = paired_terms, groups = model$groups,
                          vcov = vcov, inference = inference, alpha = alpha, scale = scale)
        }
        fit$estimator <- estimator
        if (estimator == "amce") {
          fit$scale <- "probability"
          fit$term <- NA_character_
        }
        keys <- c("estimator", "scale", "term", "type", "group_name", "reference_group",
                  "attribute", "level", "reference_level")
        order <- match_effect_rows(target, fit, keys)
        message <- attr(fit, "covariance_error")
        fields <- c("n_clusters_effect", "estimate", "std.error", "df", "statistic", "p.value",
                    "conf.low", "conf.high", "n_clusters", "rank", "vcov", "inference",
                    "failure_reason", "status")
        for (nm in fields) target[[nm]] <- fit[[nm]][order]
        target$error_message <- NA_character_
        if (!is.null(message)) target$error_message[target$failure_reason %in% "covariance_failure"] <- paste(message, collapse = "; ")
        target
      }, error = function(e) blank(target, conditionMessage(e)))
    }))
  }), envir = env)
}

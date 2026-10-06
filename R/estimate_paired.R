# Internal numerical rules are shared by population truth and sample fits.
paired_numerics <- function() {
  list(rank_tol = 1e-10, gram_rcond_min = 1e-12, estimability_tol = 1e-8,
       information_rcond_min = 1e-12, maxit = 100L, score_tol = 1e-10,
       step_tol = 1e-10, line_search_max_halvings = 30L, armijo = 1e-4,
       fallback_maxit = 1000L, fallback_factr = 1, fallback_pgtol = 1e-12)
}

# Analysis metadata, without an estimator or public latent model.
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

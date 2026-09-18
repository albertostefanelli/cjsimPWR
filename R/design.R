#' Conjoint design
#'
#' Describes the attributes of a paired conjoint design. Every attribute of both profiles in a task is
#' drawn independently and uniformly, as in a fully randomized design.
#'
#' @param levels either a vector with the number of levels of each attribute, or a list with the level
#'   labels of each attribute. Names give the attribute names; unnamed attributes are called `var_1`,
#'   `var_2`, and so on. Level counts give the labels `"0"`, `"1"`, and so on. The first level of each
#'   attribute is its reference level. Attributes keep the order given, and there is no limit on their
#'   number.
#'
#' @return A `cj_design` object: a list with the level labels of each attribute (`levels`) and the number
#'   of levels of each attribute (`n_levels`).
#' @export
#' @md
#' @examples
#' conjoint_design(c(2, 3, 4))
#' conjoint_design(list(price = c("low", "high"), quality = c("standard", "premium")))
conjoint_design <- function(levels) {
  labels <- design_levels(levels)
  structure(list(levels = labels, n_levels = lengths(labels)), class = "cj_design")
}

# Level labels of each attribute, named by attribute.
design_levels <- function(levels) {
  if (is.numeric(levels)) {
    if (length(levels) == 0 || anyNA(levels) || any(levels < 2) || any(levels != round(levels))) {
      stop("`levels` must give a whole number of at least 2 levels for every attribute.", call. = FALSE)
    }
    labels <- lapply(levels, function(l) as.character(seq_len(l) - 1))
  } else if (is.list(levels)) {
    labels <- lapply(levels, as.character)
    valid <- vapply(labels, function(l) length(l) >= 2 && !anyNA(l) && !anyDuplicated(l), logical(1))
    if (length(labels) == 0 || !all(valid)) {
      stop("every attribute in `levels` needs at least two distinct, non-missing level labels.", call. = FALSE)
    }
  } else {
    stop("`levels` must be a vector of level counts or a list of level labels.", call. = FALSE)
  }
  attrs <- names(levels)
  if (is.null(attrs)) attrs <- rep("", length(labels))
  unnamed <- is.na(attrs) | attrs == ""
  attrs[unnamed] <- paste0("var_", seq_along(labels))[unnamed]
  if (anyDuplicated(attrs)) {
    stop("attribute names must be unique.", call. = FALSE)
  }
  reserved <- c("group", "respondent", "task", "profile", "y")
  if (any(attrs %in% reserved)) {
    stop("attributes cannot be named ", paste(reserved, collapse = ", "), ".", call. = FALSE)
  }
  names(labels) <- attrs
  labels
}

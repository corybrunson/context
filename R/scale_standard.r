#' Standard scales
#' 
#' @name scale_standard
#' @param objs Vector (usually character or factor).
#' @param x Factor vector. Ignored if \code{objs} is present.
#' @param n Positive integer. The number of objects in the scale. Ignored if 
#'   \code{objs} or \code{var} is present.
#' @param scale Character. Name of standard scale, e.g. "ordinal". Defaults to
#'   "nominal".
#' @param ... Additional arguments (listed below) passed from 
#'   \code{scale_standard} to specific scale generators.
#' @param cut Numeric. Integer between 0 and \code{length(objs)} indicating the 
#'   index at which to cut \code{objs} for biordinal scaling.
#' @export
scale_standard <- function(objs, x, n, scale = "nominal", ...) {
  if (missing(objs)) {
    if (missing(x)) {
      if (missing(n)) stop("None of 'objs', 'x', and 'n' provided")
      objs <- factor(1:n, levels = 1:n)
    } else {
      if (!missing(n)) warning("'x' provided; 'n' ignored")
      objs <- levels(x)
    }
  } else {
    if (!missing(x) | !missing(n)) {
      warning("'objs', provided; 'x' and 'n' ignored")
    }
  }
  
  scale_type <- get(paste0("scale_", scale))
  scale_mat <- scale_type(objs = objs, ...)
  scale_mat
}

#' @rdname scale_standard
#' @export
scale_nominal <- function(objs) {
  scale_mat <- diag(length(objs))
  rownames(scale_mat) <- colnames(scale_mat) <- sort(objs)
  scale_mat
}

#' @rdname scale_standard
#' @export
scale_boolean <- function(objs) {
  scale_mat <- 1 - diag(length(objs))
  rownames(scale_mat) <- sort(objs)
  colnames(scale_mat) <- paste0("neq_", sort(objs))
  scale_mat
}

#' @rdname scale_standard
#' @export
scale_ordinal <- function(objs) {
  scale_mat <- upper.tri(diag(length(objs)), diag = TRUE)
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- sort(objs)
  colnames(scale_mat) <- paste0("leq_", sort(objs))
  scale_mat
}

#' @rdname scale_standard
#' @export
scale_revordinal <- function(objs) {
  scale_mat <- lower.tri(diag(length(objs)), diag = TRUE)
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- sort(objs)
  colnames(scale_mat) <- paste0("geq_", sort(objs))
  scale_mat
}

#' @rdname scale_standard
#' @export
scale_interordinal <- function(objs) {
  scale_mat <- cbind(upper.tri(diag(length(objs)), diag = TRUE),
                     lower.tri(diag(length(objs)), diag = TRUE))
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- sort(objs)
  colnames(scale_mat) <- paste0(rep(c("leq", "geq"), each = length(objs)),
                                "_", rep(objs, times = 2))
  scale_mat
}

#' @rdname scale_standard
#' @export
scale_biordinal <- function(objs, cut = ceiling(length(objs) / 2)) {
  if (cut == 0) return(scale_revordinal(objs))
  if (cut == length(objs)) return(scale_ordinal(objs))
  rem <- length(objs) - cut
  scale_mat <- rbind(cbind(upper.tri(diag(cut), diag = TRUE),
                           matrix(F, nrow = cut, ncol = rem)),
                     cbind(matrix(F, nrow = rem, ncol = cut),
                           lower.tri(diag(rem), diag = TRUE)))
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- sort(objs)
  colnames(scale_mat) <- paste0(rep(c("leq", "geq"), c(cut, rem)),
                                "_", objs)
  scale_mat
}

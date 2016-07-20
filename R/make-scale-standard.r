#' Build standard conceptual scales
#' 
#' @name make_scale_standard
#' @example inst/examples/make-scale-standard.r
#' @param objs Vector (usually character or factor).
#' @param n Positive integer. The number of objects in the scale. Ignored if 
#'   \code{objs} is present.
#' @param scale Character. Name of standard scale, e.g. "ordinal". Defaults to
#'   "nominal".
#' @param ... Additional arguments (listed below) passed from 
#'   \code{scale_standard} to specific scale generators.
#' @param cut Numeric. Integer between 0 and \code{length(objs)} indicating the 
#'   index at which to cut \code{objs} for biordinal scaling.
#' @export
make_scale_standard <- function(objs, n, scale = "nominal", ...) {
  # handle inputs
  if (missing(objs)) {
    if (missing(n)) stop("Neither 'objs' nor 'n' provided")
    objs <- factor(1:n, levels = 1:n)
  } else {
    if (!missing(n)) warning("'objs', provided; ignoring 'n'")
  }
  
  # each object should only appear once (in order of appearance)
  objs <- uniquify_objects(objs)
  
  # scaling context
  scale_type <- get(paste0("make_scale_", scale))
  scale_mat <- scale_type(objs = objs, ...)
  scale_mat
}

#' @rdname make_scale_standard
#' @export
make_scale_nominal <- function(objs) {
  objs <- uniquify_objects(objs)
  scale_mat <- diag(length(objs))
  rownames(scale_mat) <- colnames(scale_mat) <- objs
  scale_mat
}

#' @rdname make_scale_standard
#' @export
make_scale_boolean <- function(objs) {
  objs <- uniquify_objects(objs)
  scale_mat <- 1 - diag(length(objs))
  rownames(scale_mat) <- objs
  colnames(scale_mat) <- paste0("neq_", objs)
  scale_mat
}

#' @rdname make_scale_standard
#' @export
make_scale_ordinal <- function(objs) {
  objs <- uniquify_objects(objs)
  scale_mat <- upper.tri(diag(length(objs)), diag = TRUE)
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- objs
  colnames(scale_mat) <- paste0("leq_", objs)
  scale_mat
}

#' @rdname make_scale_standard
#' @export
make_scale_revordinal <- function(objs) {
  objs <- uniquify_objects(objs)
  scale_mat <- lower.tri(diag(length(objs)), diag = TRUE)
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- objs
  colnames(scale_mat) <- paste0("geq_", objs)
  scale_mat
}

#' @rdname make_scale_standard
#' @export
make_scale_interordinal <- function(objs) {
  objs <- uniquify_objects(objs)
  scale_mat <- cbind(upper.tri(diag(length(objs)), diag = TRUE),
                     lower.tri(diag(length(objs)), diag = TRUE))
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- objs
  colnames(scale_mat) <- paste0(rep(c("leq", "geq"), each = length(objs)),
                                "_", rep(objs, times = 2))
  scale_mat
}

#' @rdname make_scale_standard
#' @export
make_scale_biordinal <- function(objs, cut = ceiling(length(objs) / 2)) {
  objs <- uniquify_objects(objs)
  if (cut == 0) return(make_scale_revordinal(objs))
  if (cut == length(objs)) return(make_scale_ordinal(objs))
  rem <- length(objs) - cut
  scale_mat <- rbind(cbind(upper.tri(diag(cut), diag = TRUE),
                           matrix(F, nrow = cut, ncol = rem)),
                     cbind(matrix(F, nrow = rem, ncol = cut),
                           lower.tri(diag(rem), diag = TRUE)))
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- objs
  colnames(scale_mat) <- paste0(rep(c("leq", "geq"), c(cut, rem)),
                                "_", objs)
  scale_mat
}

uniquify_objects <- function(objs) {
  objs <- if (is.factor(objs)) {
    levs <- levels(objs)
    if (any(is.na(objs))) levs <- c(levs, NA)
    levs
  } else if (is.numeric(objs)) {
    sort(unique(objs))
  } else unique(objs)
  objs <- as.character(objs)
  if (NA %in% objs) objs[which(is.na(objs))] <- "NA"
  objs
}

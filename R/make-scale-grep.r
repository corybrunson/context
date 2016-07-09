#' Conceptual scale based on pattern matching
#' 
#' @param objs Vector (usually character or factor).
#' @param patterns Character vector of regular expressions. Each attribute of 
#'   the resulting context amounts to \code{grepl(x = objs, pattern = pattern)} 
#'   for some \code{pattern} in \code{patterns}.
#' @param split Character. Regular expression used to split \code{objs} into
#'   \code{patterns}, if \code{patterns} is not provided.
#' @return A binary matrix (formal context).
#' @export
make_scale_grep <- function(objs, patterns, split) {
  # use patterns if present; otherwise split
  if (missing(patterns)) {
    patterns <- unique(unlist(strsplit(objs, split = split)))
  } else {
    if (!missing(split)) warning("'patterns' provided; ignoring 'split")
  }

  # each object should only appear once (in order of appearance)
  objs <- unique(objs)
  
  # conceptual scale based on logical grep along patterns
  scale_mat <- sapply(patterns, grepl, x = objs)
  class(scale_mat) <- "numeric"
  rownames(scale_mat) <- objs
  scale_mat
}

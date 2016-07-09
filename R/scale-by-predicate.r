#' Expand variables in a data frame via logical conceptual scaling
#' 
#' @example inst/examples/scale-by-predicate.r
#' @param data Data frame.
#' @param predicate A logical expression involving variables in \code{data}.
#' @return A binary matrix (formal context).
#' @export
scale_by_predicate <- function(data, predicate) {
  # evaluate predicate
  col <- as.numeric(eval(substitute(predicate), envir = data))

  # return new column in matrix form
  ctx <- as.matrix(col, ncol = 1)
  rownames(ctx) <- rownames(data)
  colnames(ctx) <- as.expression(substitute(predicate))
  ctx
}

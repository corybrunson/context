#' Expand variables in a data frame via logical conceptual scaling
#' 
#' @example inst/examples/scale-by-predicate.r
#' @param data Data frame.
#' @param predicates A logical expression involving variables in \code{data}, or
#'   a list of such expressions.
#' @return A binary numeric matrix (formal context) of \code{nrow(data)} rows 
#'   and 1 column, whose entries are the truth values of \code{predicate} 
#'   applied to the rows of \code{data}.
#' @export
scale_by_predicate <- function(data, predicates) {
  # evaluate predicate(s)
  evals <- eval(substitute(predicates), envir = data)
  # organize them as a data frame
  ctx <- as.data.frame(evals)
  rownames(ctx) <- rownames(data)
  colnames(ctx) <- if (!is.list(evals)) {
    as.expression(substitute(predicates))
  } else {
    as.character(substitute(predicates))[-1]
  }
  ctx
}

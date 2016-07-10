#' Expand variables in a data frame via logical conceptual scaling
#' 
#' @example inst/examples/scale-by-predicate.r
#' @param data Data frame.
#' @param predicates A logical expression involving variables in \code{data}, or
#'   a list of such expressions. If a list, then the names of any named elements
#'   will be used as column names in the resulting context.
#' @return A binary numeric matrix (formal context) of \code{nrow(data)} rows 
#'   and 1 column, whose entries are the truth values of \code{predicate} 
#'   applied to the rows of \code{data}.
#' @export
scale_by_predicate <- function(data, predicates) {
  # substitute predicates
  subs <- substitute(predicates)
  # evaluate predicates
  evals <- eval(subs, envir = data)
  # organize them as a data frame
  ctx <- as.data.frame(evals)
  rownames(ctx) <- rownames(data)
  colnames(ctx) <- if (!is.list(evals)) {
    as.expression(subs)
  } else {
    ifelse(names(subs)[-1] != "", names(subs)[-1], as.character(subs)[-1])
  }
  ctx
}

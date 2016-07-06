#' Convert a frequency table (non-negative numeric matrix) to a matrix of the same dimensions containing at each position $(i,j)$ the value $1$ if the selected test statistic meets the provided cutoff threshold and $0$ otherwise.
#' 
#' @param x Numeric non-negative matrix.
#' @param stat Character string. The test statistic, either \code{chisq} or \code{ratio}.
#' @param thres Numeric. The cutoff threshold for the test statistic.
#' @param test Function taking one argument \code{x} and giving logical output. If provided, used to determine whether an entry is assigned the value $1$ (if returns \code{TRUE}) or $0$ (\code{FALSE}).
#' @return Binary numeric matrix.
#' @export
binarize_matrix <- function(x, stat, thres, test = NULL) {
  # ensure that x is a frequency table
  x <- as.matrix(x)
  stopifnot(all(x >= 0))
  
  # calculate the desired matrix of statistics
  stat_mat <- if (stat == "chisq") {
    matrix_chisq(x)
  } else if (stat == "ratio") {
    matrix_ratio(x)
  } else stop("Invalid 'stat' input")
  
  # if not provided, test whether the statistic is greater than the threshold
  if (is.null(test)) {
    test <- function(x) x > thres
  } else {
    if (!missing(thres)) warning("'test' function provided; ignoring 'thres'")
  }
  
  # construct binary matrix from statistic matrix by test function
  bin_mat <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  eval <- test(as.vector(stat_mat))
  bin_mat[eval == TRUE] <- 1
  bin_mat[eval == FALSE] <- 0
  bin_mat
}

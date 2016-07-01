#' Convert a frequency table (non-negative numeric matrix) to a matrix of the
#' same dimensions containing at each position \code{(i, j)} the chi-square
#' value associated with the 2-by-2 frequency table obtained by collapsing the
#' original matrix along all rows and columns other than \code{i} and \code{j}.
#' 
#' @param x Numeric non-negative matrix.
#' @export
matrix_chisq <- function(x) {
  # ensure that x is a binary matrix
  x <- as.matrix(x)
  stopifnot(all(x %in% 0:1))
  
  # initialize the chi-square matrix
  mat_chisq <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  
  # calculate the chi-square statistic for each matrix entry
  for (i in 1:nrow(mat_chisq)) {
    for(j in 1:ncol(mat_chisq)) {
      ct <- matrix(0, nrow = 2, ncol = 2)
      ct[1, 1] <- x[i,j]
      ct[2, 1] <- sum(x[, j]) - x[i, j]
      ct[1, 2] <- sum(x[i, ]) - x[i, j]
      ct[2, 2] <- sum(x) - sum(x[i, ]) - sum(x[, j]) + sum(x[i, j])
      mat_chisq[i, j] <- chisq.test(ct)$statistic
    }
  }
  
  # return the chi-square matrix
  mat_chisq
}

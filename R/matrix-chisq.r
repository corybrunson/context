#' Convert a frequency table (non-negative numeric matrix) to a matrix of the
#' same dimensions containing at each position \code{(i, j)} the chi-square
#' value associated with the 2-by-2 frequency table obtained by collapsing the
#' original matrix along all rows and columns other than \code{i} and \code{j}.
#' 
#' @param x Numeric non-negative matrix.
#' @export
matrix_chisq <- function(x) {
  # ensure that x is a frequency table
  x <- as.matrix(x)
  stopifnot(all(x >= 0))
  
  # initialize the chi-square matrix
  mat_chisq <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  
  # calculate the chi-square statistic for each matrix entry
  for (i in 1:nrow(mat_chisq)) {
    for(j in 1:ncol(mat_chisq)) {
      ct <- matrix(c(x[i, j], sum(x[-i, j]), sum(x[i, -j]), sum(x[-i, -j])),
                   nrow = 2)
      mat_chisq[i, j] <- chisq.test(ct)$statistic
    }
  }
  
  # return the chi-square matrix
  mat_chisq
}

#' Convert a frequency table (non-negative numeric matrix) to a matrix of the 
#' same dimensions containing at each position \code{(i, j)} the ratio of the
#' joint probability $P_{(i,j)}$ to the product $P_iP_j$ of the marginal
#' probabilities (based on the row and column sums).
#' 
#' @param x Numeric non-negative matrix.
#' @export
matrix_ratio <- function(x) {
  # ensure that x is a binary matrix
  x <- as.matrix(x)
  stopifnot(all(x %in% 0:1))
  
  # calculate matrix of joint probabilities
  mat_joint <- x / sum(x)
  
  # calculate matrix of products of marginal probabilities
  # (expected joint probabilities based on null hypothesis)
  mat_margprod <- as.matrix(rowSums(x) / sum(x)) %*% t(colSums(x) / sum(x))
  
  # return the element-wise ratio
  mat_joint / mat_margprod
}

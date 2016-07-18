#' Convert a frequency table (non-negative numeric matrix) to a matrix of the 
#' same dimensions containing at each position $(i,j)$ the value $1$ if the 
#' selected test statistic meets the provided cutoff threshold and $0$ 
#' otherwise. In the case of a chi-squared test, an entry in the result is 1
#' only if the corresponding entry in the input matrix was greater than
#' expected.
#' 
#' @name binarize_matrix
#' @param x Numeric non-negative matrix.
#' @param stat Character string. The test statistic, either \code{chisq} or 
#'   \code{ratio}.
#' @param thres Numeric. The cutoff threshold for the test statistic.
#' @param test Function taking one argument \code{x} and giving logical output. 
#'   If provided, used to determine whether an entry is assigned the value $1$ 
#'   (if returns \code{TRUE}) or $0$ (\code{FALSE}).
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

#' @rdname binarize_matrix
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
      # matrix of expected values, based on marginal frequencies
      ct_exp <- as.matrix(rowSums(ct)) %*% t(colSums(ct)) / sum(ct)
      # if ct[1, 1] is larger than expected, assign it the chi-square statistic;
      # otherwise, assign it the value zero
      mat_chisq[i, j] <- if (ct[1, 1] <= ct_exp[1, 1]) {
        0
      } else {
        sum(((ct - ct_exp) ^ 2) / ct_exp)
      }
    }
  }
  
  # return the chi-square matrix
  mat_chisq
}

#' @rdname binarize_matrix
#' @export
matrix_ratio <- function(x) {
  # ensure that x is a frequency table
  x <- as.matrix(x)
  stopifnot(all(x >= 0))
  
  # calculate matrix of joint probabilities
  mat_joint <- x / sum(x)
  
  # calculate matrix of products of marginal probabilities
  # (expected joint probabilities based on null hypothesis)
  mat_margprod <- as.matrix(rowSums(x) / sum(x)) %*% t(colSums(x) / sum(x))
  
  # return the element-wise ratio
  mat_joint / mat_margprod
}

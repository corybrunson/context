#' Write binary matrices to cross-table files (.csv for the concepts module in
#' Python).
#' 
#' @param x Numeric binary matrix, preferably with row and column names.
#' @param file Character. File name (with path, preferably ending in 
#'   \code{.csv}) to export formal context to.
#' @return Nothing.
#' @export
write_ct <- function(x, file = "") {
  # ensuring that the data used is in matrix form and treated as a matrix
  x <- as.matrix(x)
  
  # classify the matrix as a character matrix
  # (so ones and zeros may easily be replaced)
  class(x) <- 'character'
  
  # replace zeros with blank spaces and ones with X's
  zero.points <- which(x == "0")
  x[zero.points] <- ""
  one.points <- which(x == "1")
  x[one.points] <- "X"
  
  # create csv file that will be put in a directory based on the file name
  write.csv(x, file = file, quote = FALSE)
}

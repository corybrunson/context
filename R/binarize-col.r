#' Convert single variable columns to multiple binary columns
#' 
#' @param data Data frame.
#' @param col Character or numeric. The names or indices of the columns of
#'   \code{data} to be converted.
#' @param sep Character. The string to connect each variable to its value to
#'   create names for the binary columns.
#' @param na.value Numeric. Value to assign to NA entries in value-specific
#'   columns. (NA entries are converted to 1 in NA columns.) Defaults to 0.
#' @return A data frame having the same number of rows as \code{data}.

# function to create binary columns from factor columns
binarize_col <- function(data, col = 1:ncol(data), sep = ".", na.value = 0) {
  
  # use recursion to reduce the function to the one-variable setting
  if (length(col) > 1) {
    as.data.frame(dplyr::bind_cols(lapply(col, binarize_col, data = data)))
  } else {
    
    # all values taken by the variable, in order
    vals <- sort(unique(data[[col]]), na.last = TRUE)
    
    # data frame of binary columns for each possible value
    new_data <- as.data.frame(lapply(vals, function(v) {
      as.numeric(if (is.na(v)) is.na(data[[col]]) else {
        ifelse(is.na(data[[col]]), na.value, data[[col]] == v)
      })
    }))
    
    # names given by pasting original variable name and value
    names(new_data) <- paste0(names(data)[col], sep, vals)
    new_data
  }
}

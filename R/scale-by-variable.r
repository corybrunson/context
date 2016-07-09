#' Expand variables in a data frame via contextualized or named conceptual
#' scaling
#' 
#' @example inst/examples/scale-by-variable.r
#' @param data Data frame.
#' @param var Character. The name of the column of \code{data} that \code{scale}
#'   scales.
#' @param scale Binary numeric matrix or character. The scale to use to 
#'   transform \code{var}, either as a formal context (binary matrix) or as a 
#'   standard type (passed to \code{\link{make_scale_standard}}).
#' @return A binary matrix (formal context).
#' @export
scale_by_variable <- function(data, var, scale) {
  n <- ncol(data)
  
  if (is.character(scale)) {
    # recover standard scales
    scale <- make_scale_standard(objs = data[[var]], scale = scale)
    colnames(scale) <- paste0(var, "_", colnames(scale))
  } else {
    # ensure that the scale is a binary matrix
    stopifnot(all(scale %in% 0:1))
    # verify that the values of the variable are the objects of the scale
    stopifnot(setequal(data[[var]], rownames(scale)))
  }
  
  # merge scale into data
  scale_data <- as.data.frame(cbind(rownames(scale), scale))
  names(scale_data)[1] <- var
  data$..index.. <- 1:nrow(data)
  data <- merge(data, scale_data, by = var,
                all.x = TRUE, all.y = FALSE,
                sort = FALSE)
  data <- data[order(data$..index..), ]
  
  # return new columns in matrix form
  ctx <- as.matrix(data[, (n + 2):ncol(data)])
  class(ctx) <- "numeric"
  if (!is.null(rownames(data))) rownames(ctx) <- rownames(data)
  ctx
}

#' @rdname scale_by_variable
#' @param vars Character vector. The names of the columns of \code{data} that
#'   \code{scales} scale.
#' @param scales List having the same length as \code{vars} containing the
#'   scales to transform \code{vars}.
#' @export
scale_by_variables <- function(data, vars = names(scales), scales) {
  stopifnot(length(vars) == length(scales))
  do.call(cbind, lapply(1:length(scales), function(i) {
    scale_by_variable(data = data, var = vars[i], scale = scales[[i]])
  }))
}

#' Write binary matrices to cross-table files.
#' 
#' @name write_context
#' @param x Numeric binary matrix, preferably with row and column names.
#' @param file Character. File name (with or without extension) to export formal
#'   context to.
#' @param format Character. Formatting to use, from among "coron"/"rcf" and 
#'   "concepts".
#' @return Nothing.
#' @export
write_context <- function(x, file = "", format) {
  # disambiguate format
  format <- gsub("^\\.", "", tolower(format))
  # send to specific function
  if (format %in% c("coron", "rcf")) {
    write_context_coron(x = x, file = file)
  } else if (format %in% c("concepts")) {
    write_context_concepts(x = x, file = file)
  } else {
    stop("Unrecognized file format")
  }
}

#' @rdname write_context
write_context_coron <- function(x, file = "") {
  # ensure that x is a binary matrix
  x <- as.matrix(x)
  stopifnot(all(x %in% 0:1))
  # warn if file extension is not Coron standard
  if (substr(file, nchar(file) - 3, nchar(file)) != ".rcf") {
    warning("Not using standard Coron file extension")
  }
  
  # preamble
  cat(paste0("# Created ", Sys.time(), " using 'write_rcf'"),
      file = file, append = FALSE)
  cat("\n\n", file = file, append = TRUE)
  cat(c("[Relational Context]", "Default Name",
        "[Binary Relation]", "Name_of_dataset"),
      file = file, sep = "\n", append = TRUE)
  
  # objects
  if (is.null(rownames(x))) rownames(x) <- 1:nrow(x)
  cat(rownames(x), file = file, sep = " | ", append = TRUE)
  cat("\n", file = file, append = TRUE)
  
  # attributes
  if (is.null(colnames(x))) {
    colnames(x) <- 1:ncol(x)
    i <- 0
    while(i * 26 < ncol(x)) {
      colnames(x)[i * 26 + 1:26] <-
        apply(matrix(rep(letters, i + 1), nrow = 26), 1,
              paste0, collapse = "")
    }
  }
  cat(colnames(x), file = file, sep = " | ", append = TRUE)
  cat("\n", file = file, append = TRUE)
  
  # cross-table
  utils::write.table(x, file = file, append = TRUE, quote = FALSE,
                     row.names = FALSE, col.names = FALSE)
  
  # postamble
  cat("[END Relational Context]\n", file = file, sep = " | ", append = TRUE)
}

#' @rdname write_context
write_context_concepts <- function(x, file = "") {
  # ensuring that the data used is in matrix form and treated as a matrix
  x <- as.matrix(x)
  stopifnot(all(x %in% 0:1))
  # warn if file extension is not Coron standard
  if (substr(file, nchar(file) - 3, nchar(file)) != ".csv") {
    warning("Not using standard concepts file extension")
  }
  
  # classify the matrix as a character matrix
  # (so ones and zeros may easily be replaced)
  class(x) <- 'character'
  
  # replace zeros with blank spaces and ones with X's
  zero.points <- which(x == "0")
  x[zero.points] <- ""
  one.points <- which(x == "1")
  x[one.points] <- "X"
  
  # create csv file that will be put in a directory based on the file name
  utils::write.csv(x, file = file, quote = FALSE)
}

#' Write binary matrices to cross-table files (\code{.rcf} for Coron).
#' 
#' @param x Numeric binary matrix, preferably with row and column names.
#' @param name Character. Name given to the formal context, included in the
#'   header to the \code{.rcf} file.
#' @param file Character. File name (with path, preferably ending in
#'   \code{.rcf}) to export formal context to.
#' @return Nothing.
write_rcf <- function(x, name = "Default Name", file = "") {
    x <- as.matrix(x)
    stopifnot(all(x %in% 0:1))
    # preamble
    cat(paste0("# Created ", Sys.time(), " using 'write_rcf'"),
        file = file, append = FALSE)
    cat("\n\n", file = file, append = TRUE)
    cat(c("[Relational Context]", name,
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
    write.table(x, file = file, append = TRUE, quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    # postamble
    cat("[END Relational Context]\n", file = file, sep = " | ", append = TRUE)
}

#' Print a cross-table to be copy-pasted or sourced into a TeX file.
#' 
#' See \url{http://tex.stackexchange.com/a/32690} and
#' \url{http://tex.stackexchange.com/questions/49746/a-table-with-square-cells}.
#' 
#' @param x Numeric binary matrix.
#' @return Nothing.
#' @export
#' @examples
#' mat <- matrix(rbinom(size = 1, n = 7 * 12, p = .333), nrow = 7, ncol = 12)
#' latex_ct(x = mat)
#' colnames(mat) <- paste0("A", 1:ncol(mat))
#' latex_ct(x = mat)
latex_ct <- function(x) {
    # require x to be a binary matrix
    x <- as.matrix(x)
    stopifnot(all(x %in% 0:1))
    
    # make y a LaTeX cross-table with the same row and column names
    y <- matrix(ifelse(x == 0, "\\(\\cdot\\)", "\\(\\times\\)"),
                nrow = nrow(x), ncol = ncol(x))
    rownames(y) <- if(!is.null(rownames(x))) rownames(x) else {
        paste0("Obj ", 1:nrow(x))
    }
    colnames(y) <- if(!is.null(colnames(x))) colnames(x) else {
        paste0("Attr ", 1:ncol(x))
    }
    
    # make columns vertical if necessary
    if (any(nchar(colnames(y)) > 3)) {
        colnames(y) <- paste0("\\rot{", colnames(y), "}")
    }
    
    # format and print
    print(xtable::xtable(
        x = y,
        align = paste0("||l||", paste(rep("c", ncol(y)), collapse = "|"), "||")
    ),
    floating = FALSE, sanitize.text.function = identity,
    add.to.row = list(pos = as.list(-1:nrow(y)),
                      command = c("\\hline", rep("\\hline ", nrow(y) + 1))),
    comment = FALSE)
}

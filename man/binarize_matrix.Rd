% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binarize-matrix.r
\name{binarize_matrix}
\alias{binarize_matrix}
\alias{matrix_chisq}
\alias{matrix_ratio}
\title{Convert a frequency table (non-negative numeric matrix) to a matrix of the 
same dimensions containing at each position $(i,j)$ the value $1$ if the 
selected test statistic meets the provided cutoff threshold and $0$ 
otherwise. In the case of a chi-squared test, an entry in the result is 1
only if the corresponding entry in the input matrix was greater than
expected.}
\usage{
binarize_matrix(x, stat, thres, test = NULL)

matrix_chisq(x)

matrix_ratio(x)
}
\arguments{
\item{x}{Numeric non-negative matrix.}

\item{stat}{Character string. The test statistic, either \code{chisq} or 
\code{ratio}.}

\item{thres}{Numeric. The cutoff threshold for the test statistic.}

\item{test}{Function taking one argument \code{x} and giving logical output. 
If provided, used to determine whether an entry is assigned the value $1$ 
(if returns \code{TRUE}) or $0$ (\code{FALSE}).}
}
\value{
Binary numeric matrix.
}
\description{
Convert a frequency table (non-negative numeric matrix) to a matrix of the 
same dimensions containing at each position $(i,j)$ the value $1$ if the 
selected test statistic meets the provided cutoff threshold and $0$ 
otherwise. In the case of a chi-squared test, an entry in the result is 1
only if the corresponding entry in the input matrix was greater than
expected.
}


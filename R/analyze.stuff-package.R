#' @docType package
#' @title Basic Tools for Analyzing Datasets
#' @name analyze.stuff
#' @aliases analyze.stuff-package
#' @description This R package provides some useful tools for analyzing data in matrices and data.frames,
#'   such as functions to find the weighted mean of each column of data, add leading zeroes, or
#'   find what percent of rows are above some cutoff for each column.
#' @details  Key functions include \cr
#'  \itemize{
#'  \item \code{\link{change.fieldnames}}: Change many fieldnames using map of current to new ones
#'  \item \code{\link{calc.fields}}: Create many new calculated fields from data.frame fields by specifying a list of formulas
#'  \item \code{\link{similar.p}}, \code{\link{setdiff2}}: Compare two datasets or sets
#'  \item \code{\link{rows.above.count}}, \code{\link{rows.above.pct}}: How many rows have values above a cutoff
#'  \item \code{\link{cols.above.count}}, \code{\link{cols.above.pct}}: How many cols have values above a cutoff
#'  \item \code{\link{rowMaxs}}, \code{\link{colMaxs}}, \code{\link{rowMins}}, \code{\link{colMins}}: Max or min of each row or col in data.frame or matrix
#'  \item \code{\link{wtd.rowMeans}}, \code{\link{wtd.colMeans}}: Weighted mean of each row or col
#'  \item \code{\link{pctiles}}, \code{\link{wtd.pctiles}}: See a table of values at 100 percentiles, for each field.
#'  \item \code{\link{na.check}}, \code{\link{length2}}: How many NA or non-NA values in each column
#'  \item \code{\link{mem}}: What objects are taking up the most memory
#'  \item \code{\link{dir2}}, \code{\link{dirr}}, \code{\link{dirdirs}}: Directory listing with wildcards, just R-related files, subfolders, etc.
#'  }
#'  May add later:  \cr
#'  \itemize{
#'    \item cols.below.count
#'    \item cols.below.pct
#'    \item cols.below.which
#'    \item rows.above.count
#'    \item rows.above.pct
#'    \item rows.above.which
#'    \item rows.below.count
#'    \item rows.below.pct
#'    \item rows.below.which
#'  }
#' @author info@@ejanalysis.com <info@@ejanalysis.com>
#' @references
#'  \url{http://ejanalysis.github.io}\cr
#'  \url{http://www.ejanalysis.com}\cr
#'
#'  **Acknowledgements: \cr
#'   The package \pkg{sp} function \code{\link[sp]{spDists}}. The \link{matrixStats} package provides versions of rowMins, rowMax, colMins, colMaxs and related functions.
#'   This package could at some point provide data.frame methods that extend those, but for now it replaces them with slower versions that work on data.frames.
#'   Source: Henrik Bengtsson (2015). matrixStats: Methods that Apply to Rows and Columns of a Matrix. R package version 0.13.1-9000. \url{https://github.com/HenrikBengtsson/matrixStats}
NULL

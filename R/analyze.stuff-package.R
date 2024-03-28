


#' @title Basic Tools for Analyzing Datasets
#' @name analyze.stuff
#' @aliases analyze.stuff-package
#'
#' @importFrom utils download.file edit glob2rx head install.packages object.size read.csv write.csv
#' @importFrom stats aggregate coef line lm lowess na.exclude quantile weighted.mean
#' @importFrom graphics abline hist lines pie
#' @importFrom grDevices dev.off png
#'
#' @description This R package provides some useful tools for analyzing data in matrices and data.frames,
#'   such as functions to find the weighted mean of each column of data, add leading zeroes, or
#'   find what percent of rows are above some cutoff for each column.
#'
#' @details  Key functions include:
#'
#'  - [change.fieldnames()]: Change many fieldnames using map of current to new ones
#'  - [calc.fields()]: Create many new calculated fields from data.frame fields by specifying a list of formulas.
#'  - [similar.p()], [setdiff2()]: Compare two datasets or sets
#'  - [rows.above.count()], [rows.above.pct()]: How many rows have values above a cutoff?
#'  - [cols.above.count()], [cols.above.pct()]: How many cols have values above a cutoff?
#'  - [rowMaxs()], [colMaxs()], [rowMins()], [colMins()]: Max or min of each row or col in data.frame or matrix (but note that matrixStats package has similar functions)
#'  - [wtd.rowMeans()], [wtd.colMeans()]: Weighted mean of each row or col
#'  - [pctiles()], [wtd.pctiles()]: See a table of values at 100 percentiles, for each field.
#'  - [na.check()], [length2()]: How many NA or non-NA values in each column?
#'  - [mem()]: What objects are taking up the most memory?
#'  - [dir2()], [dirr()], [dirdirs()]: Directory listing with wildcards, just R-related files, subfolders, etc.
#'
#'  May add later:
#'
#'    - cols.below.count
#'    - cols.below.pct
#'    - cols.below.which
#'    - rows.above.count
#'    - rows.above.pct
#'    - rows.above.which
#'    - rows.below.count
#'    - rows.below.pct
#'    - rows.below.which
#'
#' @author info@@ejanalysis.com <info@@ejanalysis.com>
#' @references
#'  <http://ejanalysis.github.io>\cr
#'  <http://www.ejanalysis.com>\cr
#'
#'  **Acknowledgements: \cr
#'   The matrixStats package provides versions of rowMins, rowMax, colMins, colMaxs and related functions.
#'   This package replaces them with slower but more flexible versions that work on data.frames.
#'   Source: Henrik Bengtsson (2015). matrixStats: Methods that Apply to Rows and Columns of a Matrix.
#'   R package version 0.13.1-9000. <https://github.com/HenrikBengtsson/matrixStats>
"_PACKAGE"
NULL

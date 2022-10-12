#' @title Number of Columns with Value at or above Cutoff
#'
#' @description Find what number of columns have a value at or above some cutoff(s).
#' @details For a matrix with a few cols of related data, find what number of columns are at/above (or below) some cutoff(s).
#' Returns a vector of number indicating how many of the columns are at/above the cutoff(s).
#' Can be used in identifying places (rows) where some indicator(s) is/are at/above one or more cutoffs, threshold values.
#'
#' @param x Data.frame or matrix of numbers to be compared to cutoff value.
#'
#' @param cutoff The numeric threshold(s) or cutoff(s) to which numbers are compared.
#'   Default is arithmetic mean of row (or mean of column, if one.cut.per.col = TRUE).
#'   Usually one number.
#'   Can be a vector of same length as number of rows (if one.cut.per.col=FALSE),
#'   in which case each row can use a different cutoff.
#'   Or, if one.cut.per.col = TRUE, then cutoff should be vector as long as the number of columns,
#'   and each column is compared to its own cutoff.
#' @param one.cut.per.col Default is FALSE, which means there is just 1 cutoff same for all cases,
#'   or cutoff is vector with one per row. If TRUE then cutoff is vector with 1 per column.
#'
#' @param or.tied Logical. Default is FALSE, which means we check if number in x is greater than the cutoff (>). If TRUE, check if greater than or equal (>=).
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found. Otherwise result will be NA when a row has an NA value in any column.
#' @param below Logical. Default is FALSE. If TRUE, uses > or >= cutoff. If FALSE, uses < or <= cutoff.
#' @return Returns a vector the same size as the number of rows in x.
#' @template abovebelow
#' @examples
#' out <- cols.above.count(x<-data.frame(a=1:10, b=rep(7,10), c=7:16), cutoff=7)
#' out
#' out # default is or.tied=FALSE
#' out <- cols.above.count(data.frame(a=1:10, b=rep(7,10), c=7:16),
#'   cutoff=7, or.tied=TRUE, below=TRUE)
#' out
#' out <- cols.above.count(data.frame(a=1:10, b=rep(7,10), c=7:16) )
#'  # Compares each number in each row to the row's mean.
#' out
#' @export
cols.above.count <- function(x, cutoff, or.tied=FALSE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {
  if (is.null(dim(x))) {numcols <- 1; stop('expected data.frame as x but has only 1 dimension')} else {numcols <- dim(x)[2]}
  if (missing(cutoff)) {
    if (one.cut.per.col) {
      cutoff <- colMeans(x, na.rm = na.rm)
    } else {
      cutoff <- rowMeans(x, na.rm = na.rm)
    }
    }
  if (one.cut.per.col) {
    if (length(cutoff) != NCOL(x)) {stop('length of cutoff should be same as number of columns in x if one.cut.per.col=T')}
    x <- t(as.matrix(x)) # this allows it to compare vector of N cutoffs to N columns
  } else {
    if (length(cutoff) != NROW(x) & length(cutoff) != 1) {stop('length of cutoff should be 1 or same as number of columns in x, if one.cut.per.col=F')}
  }
  if (below) {
    if  (or.tied) { y <- ( x <= cutoff) }
    if (!or.tied) { y <- ( x <  cutoff) }
  } else {
    if  (or.tied) { y <- ( x >= cutoff) }
    if (!or.tied) { y <- ( x >  cutoff) }
  }
  if (one.cut.per.col) {y <- t(y)}
  count.per.row <- rowSums(y, na.rm = na.rm)
  return(count.per.row)
}

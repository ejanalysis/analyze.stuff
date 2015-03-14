#' @title Number of Columns with Value at or above Cutoff
#'
#' @description Find what number of columns have a value at or above some cutoff.
#' @details For a matrix with a few cols of related data, find what number of columns are at/above (or below) some cutoff.
#' Returns a vector of number indicating how many of the columns are at/above the cutoff.
#' Can be used in identifying places (rows) where some indicator(s) is/are at/above a cutoff, threshold value.
#'
#' @param x Data.frame or matrix of numbers to be compared to cutoff value.
#' @param cutoff The numeric threshold or cutoff to which numbers are compared. Default is arithmetic mean of row. Usually one number, but can be a vector of same length as number of rows, in which case each row can use a different cutoff.
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
cols.above.count <- function(x, cutoff, or.tied=FALSE, na.rm=TRUE, below=FALSE) {
  if (is.null(dim(x))) {numcols <- 1; stop('expected data.frame as x but has only 1 dimension')} else {numcols <- dim(x)[2]}
  if (missing(cutoff)) {cutoff <- rowMeans(x)}
  if (below) {
    if  (or.tied) { count.per.row <- rowSums( x <= cutoff, na.rm=na.rm) }
    if (!or.tied) { count.per.row <- rowSums( x <  cutoff, na.rm=na.rm) }
  } else {
    if  (or.tied) { count.per.row <- rowSums( x >= cutoff, na.rm=na.rm) }
    if (!or.tied) { count.per.row <- rowSums( x >  cutoff, na.rm=na.rm) }
  }
  return(count.per.row)
}

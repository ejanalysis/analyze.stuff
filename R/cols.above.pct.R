#' @title Percent of Columns with Value at or above Cutoff
#' @author author
#' @description Find what percent of columns have a value at or above some cutoff.
#' @details For a matrix with a few cols of related data, find what percent of columns are at/above (or below) some cutoff.
#' Returns a vector of number indicating what percentage of the columns are at/above the cutoff.
#' Can be used in identifying places (rows) where some indicator(s) is/are at/above a cutoff, threshold value.
#'
#' @param x Data.frame or matrix of numbers to be compared to cutoff value.
#' @param cutoff The numeric threshold or cutoff to which numbers are compared. Default is arithmetic mean of row. Usually one number, but can be a vector of same length as number of rows, in which case each row can use a different cutoff.
#' @param or.tied Logical. Default is FALSE, which means we check if number in x is greater than the cutoff (>). If TRUE, check if greater than or equal (>=).
#' @param below Logical. Default is FALSE. If TRUE, uses > or >= cutoff. If FALSE, uses < or <= cutoff.
#' @param na.rm Logical, default TRUE. Should NA values be removed before analysis.
#' @return Returns a vector the same size as the number of rows in x.
#' @seealso \code{\link{rowMaxs}} \code{\link{rowMins}} \code{\link{colMaxs}} \code{\link{pct.above}} \code{\link{pct.below}} \code{\link{cols.above.which}} \code{\link{cols.above.pct}}
#' @examples
#' out <- cols.above.pct(x<-data.frame(a=1:10, b=rep(7,10), c=7:16), cutoff=7)
#' out
#' out # default is or.tied=FALSE
#' out <- cols.above.pct(data.frame(a=1:10, b=rep(7,10), c=7:16), cutoff=7, or.tied=TRUE, below=TRUE)
#' out
#' out <- cols.above.pct(data.frame(a=1:10, b=rep(7,10), c=7:16) )  # Compares each number in each row to the row's mean.
#' out
#' @note Future work: these functions could have wts, na.rm, & allow cutoffs or benchmarks as a vector (not just 1 number), & have benchnames.
#' @export
cols.above.pct <- function(x, cutoff, or.tied=FALSE, na.rm=TRUE, below=FALSE) {
  count.per.row <- cols.above.count(x=x, cutoff=cutoff, or.tied=or.tied, na.rm=na.rm, below=below)
  pct.of.cols.per.row <- count.per.row / numcols
  return(pct.of.cols.per.row)
}

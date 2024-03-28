#' @title Does each Row have a Value at or above Cutoff(s)
#'
#' @description Flag which cells are at or above some cutoff(s) or mean.
#' @details For a matrix with a few cols of related data, find which cells are at/above (or below) some cutoff.
#' Returns a logical matrix, with TRUE for each cell that is at/above the cutoff.
#' Can be used in identifying places (rows) where some indicator(s) is/are at/above a cutoff, threshold value.
#'
#' @param x Data.frame or matrix of numbers to be compared to cutoff value.
#' @param cutoff The numeric threshold or cutoff to which numbers are compared. Default is arithmetic mean of row. Usually one number, but can be a vector of same length as number of rows, in which case each row can use a different cutoff.
#' @param or.tied Logical. Default is FALSE, which means we check if number in x is greater than the cutoff (>). If TRUE, check if greater than or equal (>=).
#' @param below Logical. Default is FALSE. If TRUE, uses > or >= cutoff. If FALSE, uses < or <= cutoff.
#' @return Returns a logical matrix the same size as x. ** Note this is different than [which()] -- That function returns the positions of TRUE elements but this returns TRUE or FALSE for all elements.
#' @seealso [count.above()]  [pct.above()] [pct.below()] to see, for each column, the count or percent of rows that have values above or below a cutoff.
#' @seealso [cols.above.count()] [cols.above.which()] [cols.above.pct()] to see, for each row, the count or which or fraction of columns with numbers at/above/below cutoff.
#' @seealso [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]  [tablefixed()]
#' 
#' @examples
#' out <- cols.above.which(x<-data.frame(a=1:10, b=rep(7,10), c=7:16), cutoff=7)
#' out
#' out # default is or.tied=FALSE
#' out <- cols.above.which(data.frame(a=1:10, b=rep(7,10), c=7:16),
#'   cutoff=7, or.tied=TRUE, below=TRUE)
#' out
#' out <- cols.above.which(data.frame(a=1:10, b=rep(7,10), c=7:16) )
#'  # Compares each number in each row to the row's mean.
#' out
#'
#' @export
#'
rows.above.which <- function(x, cutoff, or.tied=FALSE, below=FALSE) {

  if (is.null(dim(x))) {stop('expected data.frame or matrix as x but has only 1 dimension')}
  if (missing(cutoff)) {cutoff <- colMeans(x)}
  if (below) {
    if (or.tied)  {logical.matrix <- (x <= cutoff) }
    if (!or.tied) {logical.matrix <- (x <  cutoff) }
  } else {
    if (or.tied)  {logical.matrix <- (x >= cutoff) }
    if (!or.tied) {logical.matrix <- (x >  cutoff) }
  }
  # check to verify it returns df for df input and matrix for matrix input
  return(logical.matrix)
}

#' @title Weighted Sum of each Row
#' @description
#' Returns weighted sum of each row of a data.frame or matrix, based on specified weights, one weight per column.
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of columns
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found. Otherwise result will be NA when any NA is in a vector.
#' @return Returns a vector of numbers of length equal to number of rows in df.
#' @seealso \code{\link{wtd.rowMeans}} \code{\link{rowMaxs}} \code{\link{rowMins}} \code{\link{colMins}}
#' @examples
#' x=data.frame(a=c(NA, 2:10), b=rep(100,10))
#' w=c(1.1, 2)
#' cbind(x, wtd.rowSums(x, w) )
#' @export
wtd.rowSums <- function(x, wts=1, na.rm=TRUE) {
  rowSums(t(t(x) * wts), na.rm=na.rm)
}
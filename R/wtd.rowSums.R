#' @title Weighted Sum of each Row
#' @description
#' Returns weighted sum of each row of a data.frame or matrix, based on specified weights, one weight per column.
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of columns
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found. Otherwise result will be NA when any NA is in a vector.
#' @return Returns a vector of numbers of length equal to number of rows in df.
#' @template meansum
#' @export
wtd.rowSums <- function(x, wts=1, na.rm=TRUE) {
  rowSums(t(t(x) * wts), na.rm=na.rm)
}

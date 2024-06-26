#' @title Weighted Mean of each Row - WORK IN PROGRESS
#' @description
#' Returns weighted mean of each row of a data.frame or matrix, based on specified weights, one weight per column.
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of columns
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found. Otherwise result will be NA when any NA is in a vector.
#' @param dims dims=1 is default. Not used. integer: Which dimensions are regarded as 'rows' or 'columns' to sum over. For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
#' @return Returns a vector of numbers of length equal to number of rows in df.
#' @seealso [wtd.colMeans()] [wtd.rowMeans()] [wtd.rowSums()] [rowMaxs()] [rowMins()] [colMins()]
#' @examples
#' x=data.frame(a=c(NA, 2:10), b=rep(100,10), c=rep(3,10))
#' w=c(1.1, 2, NA)
#' cbind(x, wtd.rowMeans(x, w) )
#' cbind(x, wtd.rowSums(x, w) )
#' x=data.frame(a=c(NA, 2:4), b=rep(100,4), c=rep(3,4))
#' w=c(1.1, 2, NA, 0)
#' print(cbind(x,w, wtd=w*x))
#' print(wtd.colMeans(x, w, na.rm=TRUE))
#' #rbind(cbind(x,w,wtd=w*x), c(wtd.colMeans(x,w,na.rm=TRUE), 'wtd.colMeans', rep(NA,length(w))))
#'
#' x=data.frame(a=c(NA, 2:10), b=rep(100,10), c=rep(3,10))
#' w=c(1.1, 2, NA, rep(1, 7))
#' print(cbind(x,w, wtd=w*x))
#' rbind(cbind(x, w), cbind(wtd.colMeans(x, w, na.rm=TRUE), w='wtd.colMeans') )
#' print(w*cbind(x,w))
#'
#' @export
#'
wtd.rowMeans <- function(x, wts=1, na.rm = FALSE, dims = 1) {

  warning(' **** THIS IS WORK IN PROGRESS AS IS wtd.colMeans() !!! ****')

  rowMeans(t(t(x) * wts), na.rm = na.rm) * length(wts) / sum(wts, na.rm = na.rm)

  # this might not work right handling NA VALUES IN wts vs in x ???****
  # instead of length(w) might want length2(w, na.rm = na.rm) or just na.rm=TRUE ???

  # *** Question: How do we want NA values in x or in wts or in both treated?
  # Currently, one NA among the wts will make all the results NA, if na.rm=FALSE (default)
  # Might want

  # from base package:
  #
  # Notice that omission of missing values is done on a per-column or per-row basis,
  # so column means may not be over the same set of rows, and vice versa.
  # To use only complete rows or columns, first select them with na.omit or complete.cases (possibly on the transpose of x).
  #
  # x
  # an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
  #
  # na.rm
  # logical. Should missing values (including NaN) be omitted from the calculations?
  #
  # dims
  # integer: Which dimensions are regarded as 'rows' or 'columns' to sum over. For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
  #
  # colSums (x, na.rm = FALSE, dims = 1)
  # rowSums (x, na.rm = FALSE, dims = 1)
  # colMeans(x, na.rm = FALSE, dims = 1)
  # rowMeans(x, na.rm = FALSE, dims = 1)

}

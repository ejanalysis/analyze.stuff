#' @title Weighted Mean of each Column - WORK IN PROGRESS
#' @description
#' Returns weighted mean of each column of a data.frame or matrix, based on specified weights, one weight per row.
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of rows
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found. Otherwise result will be NA when any NA is in a vector.
#' @param dims dims=1 is default. Not used. integer: Which dimensions are regarded as 'rows' or 'columns' to sum over. For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
#' @return Returns a vector of numbers of length equal to number of columns in df.
#' @seealso \code{\link{wtd.rowMeans}} \code{\link{rowMaxs}} \code{\link{rowMins}} \code{\link{colMins}}
#' @examples
#' x=data.frame(a=c(NA, 2:4), b=rep(100,4), c=rep(3,4))
#' w=c(1.1, 2, NA, 0)
#' print(cbind(x,w, wtd=w*x))
#' print(wtd.colMeans(x, w, na.rm=TRUE))
#' #rbind(cbind(x, w, wtd=w*x), c(wtd.colMeans(x, w, na.rm=TRUE), 'wtd.colMeans', rep(NA,length(w))) )
#'
#' x=data.frame(a=c(NA, 2:10), b=rep(100,10), c=rep(3,10))
#' w=c(1.1, 2, NA, rep(1, 7))
#' print(cbind(x,w, wtd=w*x))
#' rbind(cbind(x, w), c(wtd.colMeans(x, w, na.rm=TRUE), 'wtd.colMeans') )
#' print(w*cbind(x,w))
#' @export
wtd.colMeans <- function(x, wts=1, na.rm = FALSE, dims = 1) {

  warning(' **** THIS IS WORK IN PROGRESS AS IS wtd.rowMeans() !!! Check how NA values are handled ****')

  return( colMeans(x * t(wts), na.rm=na.rm) * colSums(!is.na(x)) / sum(wts, na.rm=na.rm) )

  #colSums(!is.na(x)) instead of length(w) might use all rows of given col where value in that col is not NA
  # t(wtd.rowMeans(t(x), wts, na.rm=na.rm, dims=dims)) #problem: treats value as zero if any in row is NA? ? ?
  # this might not work right handling NA VALUES IN wts vs in x ???****
  # instead of length(w) might want length2(w, na.rm=na.rm) or just na.rm=TRUE ???

  # *** Question:  see notes in wtd.rowMeans()

  # x
  # an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
  #
  # na.rm
  # logical. Should missing values (including NaN) be omitted from the calculations?
  #
  # dims
  # integer: Which dimensions are regarded as 'rows' or 'columns' to sum over. For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
  #
}

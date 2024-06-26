#' @title Weighted Sum of each Row
#' @description
#' Returns weighted sum of each row of a data.frame or matrix, based on specified weights, one weight per column.
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of columns
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found. Otherwise result will be NA when any NA is in a vector.
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
wtd.rowSums <- function(x, wts = 1, na.rm = TRUE) {
  rowSums(t(t(x) * wts), na.rm = na.rm)
}

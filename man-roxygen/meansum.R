#' @seealso \code{\link{wtd.colMeans}} \code{\link{wtd.rowMeans}} \code{\link{wtd.rowSums}} \code{\link{rowMaxs}} \code{\link{rowMins}} \code{\link{colMins}}
#' @examples
#' x=data.frame(a=c(NA, 2:10), b=rep(100,10), c=rep(3,10))
#' w=c(1.1, 2, NA)
#' cbind(x, wtd.rowMeans(x, w) )
#' cbind(x, wtd.rowSums(x, w) )
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

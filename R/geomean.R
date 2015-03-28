#' @title Geometric mean
#'
#' @description
#' Returns the geometric mean of a vector of numbers, which is the nth root of their product.
#' @details
#' The geomean is one type of average, used in working with lognormal distributions, for example.
#' Is not as strongly influenced by extreme outliers as the arithmetic mean.
#' See \url{http://en.wikipedia.org/wiki/Geometric_mean} for many applications.
#' @param x Vector of numbers, required.
#' @param na.rm Logical value, optional, FALSE by default. If FALSE, result is NA if any of the values in x is NA. If TRUE, remove the NA values first.
#' @return Returns a single number that is the geometric mean of the numbers in x.
#' @seealso \code{\link{harmean}}  \code{\link{mean}}  \code{\link{rms}}
#' @examples
#' geomean(c(4,9)) # is the square root of 4 * 9
#' @export
geomean <- function(x, na.rm=FALSE) {
  if (!missing(na.rm) & is.numeric(na.rm)) {stop('a single vector, x, must be specified')}
  if(na.rm) {n=sum(!is.na(x))} else {n=length(x)}; return(prod(x, na.rm=na.rm)^(1 / n))
}

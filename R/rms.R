#' @title Root Mean Square (RMS), or Quadratic Mean
#'
#' @description
#' Returns the RMS, or quadratic mean of a vector of numbers.
#' @details
#' The quadratic mean is one type of average. It is the square root of the arithmetic mean of the squares.
#' See \url{http://en.wikipedia.org/wiki/Root_mean_square} or \url{http://mathworld.wolfram.com/Root-Mean-Square.html} for many applications
#' @param x Vector of numbers, required.
#' @param na.rm Logical value, optional, FALSE by default. If FALSE, result is NA if any of the values in x is NA. If TRUE, remove the NA values first.
#' @return Returns a single number
#' @seealso \code{\link{geomean}}  \code{\link{mean}}  \code{\link{harmean}} 
#' @examples
#' rms(c(1,2,4)) 
#' @export
rms <- function(x, na.rm=FALSE) {
  if (!missing(na.rm) & is.numeric(na.rm)) {stop('a single vector, x, must be specified')}
  sqrt(mean(x^2, na.rm=na.rm))
}

#' @title Harmonic mean
#'
#' @description
#' Returns the harmonic mean of a vector of numbers.
#' @details
#' The harmonic mean is one type of average. It is the reciprocal of the arithmetic mean of the reciprocals.
#' See <http://en.wikipedia.org/wiki/Harmonic_mean> for many applications of the harmonic mean.
#' @param x Vector of numbers, required.
#' @param na.rm Logical value, optional, FALSE by default. If FALSE, result is NA if any of the values in x is NA. If TRUE, remove the NA values first.
#' @return Returns a single number
#' @seealso [geomean()]  [mean()]  [rms()]
#' @examples
#' harmean(c(1,2,4))
#' @export
harmean <- function(x, na.rm=FALSE) {
  if (!missing(na.rm) & is.numeric(na.rm)) {stop('a single vector, x, must be specified')}
  if(na.rm) {n=sum(!is.na(x))} else {n=length(x)}
  return(n / sum(1/x, na.rm=na.rm))
  # harmonic mean
}

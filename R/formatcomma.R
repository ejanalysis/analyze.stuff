#' @title Print numbers with commas and 0-2 decimal places
#' @description Wrapper for \code{\link{format}} making it easier to use with my typical settings
#' @param x Numeric vector
#' @param big.m Default is a comma at every three digits (1,000,000)
#' @param nsmall Default is 2 digits after the decimal (3.14)
#' @param drop0trailing Default is FALSE, which means zeroes after the last nonzero digit in the decimal portion are still shown (TRUE would mean 1.2 is shown, not 1.20)
#' @return Same as \code{\link{format}} but with some preselected defaults
#' @seealso \code{\link{format}} 
#' @examples
#'  cbind(mynum=formatcomma(c(1452345, 1.2, 4234.12345)))
#'  cbind(mynum=formatcomma(c(1452345, 1.2, 4234.12345), drop0trailing=FALSE))
#' @export
formatcomma <- function(x, big.m=',', nsmall=2, drop0trailing=FALSE, ...) {
  format(x, big.m=big.m, nsmall=nsmall, drop0trailing=drop0trailing, ...)
}

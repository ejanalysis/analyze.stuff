#' @title Find minimum non-zero number(s)
#'
#' @description
#' Returns minimum nonzero numbers in vector, matrix, or data.frame
#'
#' @param mydf Required. Must be vector, matrix, or data.frame
#' @return A number or vector of numbers
#' @examples
#' minNonzero(-1:6)
#' minNonzero(data.frame(a=0:10, b=1:11, c=c(0,1,NA)))
#' @export
minNonzero <- function(mydf) {

  if (is.data.frame(mydf)) {
    return( sapply(mydf[ , sapply(mydf, mode)=='numeric'], function(x) {zero <- (!x);   ifelse(all(zero) || all(is.na(x)), NA, min(x[!zero],  na.rm=TRUE))})  )
  }

  if (is.matrix(mydf)) {
    return( apply(mydf[ , apply(mydf, 2, mode)=='numeric'], 2, function(x) {zero <- (!x);   ifelse(all(zero) || all(is.na(x)), NA, min(x[!zero],  na.rm=TRUE))})  )
  }

  if (is.vector(mydf)) {
    if (!is.numeric(mydf)) {stop('must be numeric')}
    zero <- (!mydf)
    return( ifelse(all(zero) || all(is.na(mydf)), NA, min(mydf[!zero],  na.rm=TRUE)) )
  }

  stop('must be vector, matrix, or data.frame')
}

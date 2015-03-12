#' @title Returns the min value of each row of a data.frame or matrix
#'
#' @description
#' Returns minimum value of each row of a data.frame or matrix.
#' @details
#' ** Note than max() and min() default to na.rm=FALSE, but this function defaults to na.rm=TRUE because that just seems more frequently useful
#' \cr\cr
#' Also see the \pkg{matrixStats} package which uses some of the same names for some functions.
#' \cr\cr
#' Based on how min() and max() behave, return Inf or -Inf if no non-missing arguments to min or max respectively?
#' to call this and suppress that warning, use suppressWarnings( f(x) )
#' \cr\cr
#' NOTE: max() and min() & this function will handle character elements by coercing all others to character (see the help for Comparison \url{http://127.0.0.1:45798/help/library/base/help/Comparison})
#' which can be confusing -- e.g., note that min(c(8,10,'txt')) returns '10' not '8' and max returns 'txt'
#' \cr\cr
#' @param df Data.frame or matrix, required.
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed first. Otherwise min will be NA when any NA is in the given vector.
#' @return Returns a vector of numbers of length equal to number of rows in df.
#' @seealso \code{\link{rowMaxs}} \code{\link{rowMins}} \code{\link{colMaxs}} \code{\link{count.above}} \code{\link{cols.above.which}} \code{\link{cols.above.pct}}
#' @examples
#' blah <- rbind(NA, data.frame(a=c(0, 0:8), b=c(0.1+(0:9)), c=c(1:10), d=c(rep(NA, 10)), e=TRUE, f=factor('factor'), g='words', stringsAsFactors=FALSE) )
#' cbind(blah, min=rowMins(blah), max=rowMaxs(blah))
#' rbind(blah, min=colMins(blah), max=colMaxs(blah))
#' blah <- blah[ , sapply(blah, function(x) is.numeric(x) || is.logical(x)) ]
#' cbind(blah, min=rowMins(blah), max=rowMaxs(blah), mean=rowMeans(blah, na.rm=TRUE), sum=rowSums(blah, na.rm=TRUE))
#' rbind(blah, min=colMins(blah), max=colMaxs(blah), mean=colMeans(blah, na.rm=TRUE), sum=colSums(blah, na.rm=TRUE))
#'\cr\cr
# had been doing this:
# mymin <- do.call(pmin, c(bg[,names.ej.pctile], na.rm=TRUE))
# mymax <- do.call(pmax, c(bg[,names.ej.pctile], na.rm=TRUE))
# now can do this:
# mymin <- rowMins(bg[,names.ej.pctile])
# mymax <- rowMaxs(bg[,names.ej.pctile])
#' @export
rowMins <- function(df, na.rm=TRUE) {
  if (is.matrix(df)) {df <- data.frame(df, stringsAsFactors=FALSE)}

  valid.cols <- sapply(df, function(x) { is.numeric(x) || is.logical(x) || is.character(x)})
  stopifnot(any(valid.cols))
  # or could just return NA?:
  # if (!any(valid.cols)) {return(NA)}
  if (any(!valid.cols) ) {warning('using only numeric (double or integer) or logical or character columns -- ignoring other columns ')}

  result <- do.call(pmin, c(df[ , valid.cols], na.rm=na.rm))

  result[nononmissing <- rowSums(!is.na(df[ , valid.cols]))==0] <- Inf
  if (any(nononmissing)) {warning('where no non-missing arguments, returning Inf')}
  return(result)

  # df = data.frame of numeric values, i.e. a list of vectors passed to pmin
  # Value returned is vector, each element is min of a row of df
}

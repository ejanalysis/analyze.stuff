#' @title Get the max value of each column of a data.frame or matrix
#'
#' @description
#' Returns maximum value of each column of a data.frame or matrix.
#' @details
#' ** Note than max() and min() default to na.rm=FALSE, but this function defaults to na.rm=TRUE because that just seems more frequently useful
#' \cr\cr
#' Note if it were just as max() and min(), cols that are factors would make this fail, even if as.character() of the factor col would return a valid numeric vector
#'  To fix that, did this:
#'  if (is.factor(x)) {x<-as.numeric(as.character(x))}
#' Also see the \pkg{matrixStats} package which uses some of the same names for some functions.
#' \cr\cr
#' Based on how min() and max() behave, return Inf or -Inf if no non-missing arguments to min or max respectively?
#' to call this and suppress that warning, use suppressWarnings( f(x) )
#' \cr\cr
#' NOTE: max() and min() & this function will handle character elements by coercing all others to character (see the help for Comparison \url{http://127.0.0.1:45798/help/library/base/help/Comparison})
#' which can be confusing -- e.g., note that min(c(8,10,'txt')) returns '10' not '8' and max returns 'txt'
#' @param df Data.frame or matrix, required.
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before max is found. Otherwise max will be NA when any NA is in a col.
#' @return Returns a vector of numbers of length equal to number of columns in df.
#' @seealso \code{\link{rowMaxs}} \code{\link{rowMins}} \code{\link{colMins}} \code{\link{count.above}} \code{\link{cols.above.which}} \code{\link{cols.above.pct}}
#' @examples
#' blah <- rbind(NA, data.frame(a=c(0, 0:8), b=c(0.1+(0:9)), c=c(1:10), d=c(rep(NA, 10)), e=TRUE, f=factor('factor'), g='words', stringsAsFactors=FALSE) )
#' cbind(blah, min=rowMins(blah), max=rowMaxs(blah))
#' rbind(blah, min=colMins(blah), max=colMaxs(blah))
#' blah <- blah[ , sapply(blah, function(x) is.numeric(x) || is.logical(x)) ]
#' cbind(blah, min=rowMins(blah), max=rowMaxs(blah), mean=rowMeans(blah, na.rm=TRUE), sum=rowSums(blah, na.rm=TRUE))
#' rbind(blah, min=colMins(blah), max=colMaxs(blah), mean=colMeans(blah, na.rm=TRUE), sum=colSums(blah, na.rm=TRUE))
#' @export
colMaxs <- function(df, na.rm=TRUE) {
  if (is.matrix(df)) {
    return( apply(df, 2, function(x) {
      if (is.factor(x)) {x<-as.numeric(as.character(x))} # finds min of numbers or characters representing numbers, but stored as factors
      max(x, na.rm=na.rm)
    } ) )
  } else {
    return( sapply(df,   function(x) {
      if (is.factor(x)) {x<-as.numeric(as.character(x))} # finds min of numbers or characters representing numbers, but stored as factors
      max(x, na.rm=na.rm)
    } ) )
  }
}

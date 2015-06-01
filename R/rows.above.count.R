#' @title Number or percent of rows (for each col) where value exceeds cutoff(s)
#'
#' @description
#' Alias for \code{\link{count.above}}
#' @param df Data.frame or matrix, required.
#' @param benchmarks Default is 'mean' but otherwise this must be a number or numeric vector of thresholds to compare values to.
#' @param benchnames Default is 'cutoff' and this string is used to create colnames for the results, such as above.cutoff.for.field1
#' @param or.tied Logical, FALSE by default, reporting on those > cutoff. But, if or.tied=TRUE, this reports on those >= cutoff.
#' @param below Logical, FALSE by default, which counts how many are above cutoff (or tied if or.tied). If TRUE, counts how many are below (or tied with) cutoff.
#' @param wts Number or vector, default is 1. Length must be a factor of number of rows in df, so length(df[,1]) is an integer multiple of length(wts)  Applies weights to when counting how many.
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed first. Otherwise result will be NA when any NA is in a col.
#' @return Returns a vector of numbers of length equal to number of columns in df.
#' @template abovebelow
#' @export
rows.above.count <- function(df, benchmarks='mean', benchnames='cutoff', or.tied=FALSE, below=FALSE, wts=1, na.rm=TRUE ) {
  count.above(df=df, benchmarks=benchmarks, benchnames=benchnames, or.tied=or.tied, below=below, wts=wts, na.rm=na.rm )
}

#' @title Number or percent of rows (for each col) where value is below cutoff(s)
#'
#' @description
#' Count the number or percent of rows (for each col of a data.frame) where the value is below some specified cutoff(s)
#' @details
#' See \code{\link{pct.above}} for details, for which this is a wrapper.
#' @param df Data.frame or matrix, required.
#' @param benchmarks Default is 'mean' but otherwise this must be a number or numeric vector of thresholds to compare values to.
#' @param benchnames Default is 'cutoff' and this string is used to create colnames for the results
#' @param or.tied Logical, FALSE by default, reporting on those < cutoff. But, if or.tied=TRUE, this reports on those <= cutoff.
#' @param below Logical, TRUE by default, which counts how many are below cutoff (or tied if or.tied). If FALSE, counts how many are above (or tied with) cutoff.
#' @param wts Number or vector, default is 1. Length must be a factor of number of rows in df, so length(df[,1]) is an integer multiple of length(wts)  Applies weights to when counting how many.
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed first. Otherwise result will be NA when any NA is in a col.
#' @param of.what Optional, character, 'all' by default, defines xxx as the text used in "pct.above.xxx" (or below) for fieldnames in results
#' @return Returns a vector of numbers of length equal to number of columns in df.
#' @template abovebelow
#' @export
pct.below <- function(df, benchmarks='mean', benchnames='cutoff', na.rm=FALSE, or.tied=FALSE, below=TRUE, wts=1, of.what='all') {
  return(pct.above(df=df, benchmarks=benchmarks, benchnames=benchnames, na.rm=na.rm, or.tied=or.tied, below=below, wts=wts, of.what=of.what))
}

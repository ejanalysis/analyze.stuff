#' @title Normalize raw scores as ratio of score to wtd mean
#' @description Provides a data.frame that takes the matrix or data.frame
#' and finds the weighted mean of each column and then divides each column of values by the column's weighted mean.
#' @details Uses [scale()]
#' @param df numeric Data.frame of one or more columns of values to be normalized, or matrix or vector to be coerced to data.frame
#' @param wts numeric Weights to use when computing weighted mean of given column, one weight per row in df (default=1) or per element of vector df. If omitted, default is unweighted mean.
#' @param na.rm logical Whether to exclude rows where weight or value or both = NA.
#' @return matrix same size as df, but with all values in given column divided by weighted mean of that column
#' @seealso [scale()]
#' @examples \dontrun{
#'  mydf_norm <- tbd
#'  ## #
#' }
#' @export
normalized <- function(df, wts=NULL, na.rm=TRUE) {
  if (is.matrix(df)) {df <- as.data.frame(df)}
  if (is.vector(df)) {df <- as.data.frame(df)}
  if (!is.null(wts) & NROW(df)!=length(wts)) {stop('wts must be same length as number of rows in df (or elements of df if it is a vector)')}

  scale(df, sapply(df, FUN=function(x) {Hmisc::wtd.mean(x, wts, na.rm=na.rm)}), center=FALSE)
}

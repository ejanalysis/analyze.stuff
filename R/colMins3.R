#' @title Returns the min value of each column of a data.frame or matrix
#'
#' @description
#' Returns minimum value of each column of a data.frame or matrix.
#' @param df data.frame or matrix
#' @param na.rm TRUE by default. Should NA values be removed first
#' @return vector of numbers with length equal to number of cols in df
#' @template maxmin
#' @export
colMins3 <- function(df, na.rm=TRUE) {
  if (is.matrix(df)) {
    return( apply(df, 2, function(x) min(x, na.rm=na.rm)) )
    # actually you could just use this for data.frame too
  } else {
    return( sapply(df, function(x) min(x, na.rm=TRUE)) )
  }
}

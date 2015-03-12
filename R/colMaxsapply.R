#' @title Get the max value of each column of a data.frame or matrix using apply
#'
#' @description
#' Returns maximum value of each column of a data.frame or matrix. For checking speed of this method vs others.
#' @param df data.frame or matrix
#' @param na.rm TRUE by default. Should NA values be removed first
#' @return vector of numbers with length equal to number of cols in df
#' @template maxmin
#' @export
colMaxsapply <- function(df, na.rm=TRUE) {

  if (is.matrix(df)) {

    return( apply(df, 2, function(x) {
      max(x, na.rm=na.rm)
      }) )
  } else {
    df <- factor.as.numeric(df)
    sapply(df, function(x) {max(x, na.rm=na.rm)} )
  }
}

#' @title Returns the min value of each column of a data.frame or matrix
#'
#' @description
#' Returns minimum value of each column of a data.frame or matrix.
#' @param df data.frame or matrix
#' @param na.rm TRUE by default. Should NA values be removed first
#' @return vector of numbers with length equal to number of cols in df
#' @template maxmin
#' @export
colMins <- function(df, na.rm=TRUE) {
  if (is.matrix(df)) {
    return( apply(df, 2, function(x) {
      if (is.factor(x)) {x<-as.numeric(as.character(x))} # finds min of numbers or characters representing numbers, but stored as factors
      min(x, na.rm=na.rm)
    } ) )
  } else {
    return( sapply(df,   function(x) {
      if (is.factor(x)) {x<-as.numeric(as.character(x))} # finds min of numbers or characters representing numbers, but stored as factors
      min(x, na.rm=na.rm)
    } ) )
  }
}


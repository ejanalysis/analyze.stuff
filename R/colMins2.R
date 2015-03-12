#' @title Returns the min value of each column of a data.frame or matrix
#'
#' @description
#' Returns minimum value of each column of a data.frame or matrix.
#' @param df data.frame or matrix
#' @param na.rm TRUE by default. Should NA values be removed first
#' @return vector of numbers with length equal to number of cols in df
#' @template maxmin
#' @export
colMins2 <- function(df, na.rm=TRUE) {
  if (is.matrix(df)) {
    return( apply(df, 2, function(x) min(x, na.rm=na.rm)) )
  } else {
    #return( rowMins( as.data.frame(t(df)))) # this as.data.frame() creating >200k lists or cols is MUCH slower than colMins just using sapply!!
    return( rowMins( as.data.frame(t(df)))) # this as.data.frame() creating >200k lists or cols is MUCH slower than colMins just using sapply!!
    #return( sapply(df, function(x) min(x, na.rm=TRUE)) )
  }
}


#' @title Get the max value of each column of a data.frame or matrix
#'
#' @description
#' Returns maximum value of each column of a data.frame or matrix.
#' @param df data.frame or matrix
#' @param na.rm TRUE by default. Should NA values be removed first
#' @return vector of numbers with length equal to number of cols in df
#' @template maxmin
#' @export
colMaxs <- function(df, na.rm=TRUE) {
  if (is.matrix(df)) {
    return( apply(df, 2, function(x) {
      max(x, na.rm=na.rm)
    } ) )
  } else {
    # converts factors storing numbers to numbers, and factors storing character strings to character, which max likes better
    df <- factor.as.numeric(df, stringsAsFactors = FALSE )
    return( sapply(df,   function(x) {
      max(x, na.rm=na.rm)
    } ) )
  }
}

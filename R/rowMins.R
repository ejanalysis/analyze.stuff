#' @title Returns the min value of each row of a data.frame or matrix
#'
#' @description
#' Returns minimum value of each row of a data.frame or matrix.
#' @param df Data.frame or matrix, required.
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed first. Otherwise min will be NA when any NA is in the given vector.
#' @return Returns a vector of numbers of length equal to number of rows in df.
#' @template maxmin
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

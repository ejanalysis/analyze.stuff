#' @title Returns the max value of each row of a data.frame or matrix
#'
#' @description
#' Returns maximum value of each row of a data.frame or matrix.
#'   pmax
#' @details This uses do.call(pmax.int). pmax and pmin take one or more vectors as arguments,
#'   recycle them to common length and return a single vector giving
#'   the ‘parallel’ maxima (or minima) of the argument vectors.
#'   pmax.int and pmin.int are faster internal versions only used when all arguments are atomic vectors and there are no classes: they drop all attributes.
#'   (Note that pmax and pmax.int both fail for raw and complex vectors since these have no ordering.)
#'   matrixStats::rowMaxs works for matrix only, not data.frame.
#'
#' @param df Data.frame or matrix, required.
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed first. Otherwise result will be NA when any NA is in the given vector.
#' @return Returns a vector of numbers of length equal to number of rows in df.
#' @template maxmin
#' @export
rowMaxs <- function(df, na.rm=TRUE) {

  if (is.matrix(df)) {df <- data.frame(df, stringsAsFactors=FALSE, drop = FALSE)}

  valid.cols <- sapply(df, function(x) { is.numeric(x) || is.logical(x) || is.character(x)})
  stopifnot(any(valid.cols))
  # or could just return NA?:
  # if (!any(valid.cols)) {return(NA)}
  if (any(!valid.cols) ) {warning('using only numeric (double or integer) or logical or character columns -- ignoring other columns ')}

  result <- do.call(pmax.int, c(df[ , valid.cols, drop = FALSE], na.rm=na.rm))

  result[nononmissing <- rowSums(!is.na(df[ , valid.cols, drop = FALSE]))==0] <- -Inf
  if (any(nononmissing)) {warning('where no non-missing arguments, returning -Inf')}
  return(result)

  # df = data.frame of numeric values, i.e. a list of vectors passed to pmax
  # Value returned is vector, each element is max of a row of df
}

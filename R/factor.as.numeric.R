#' @title Handle Numbers Stored as Factors
#'
#' @description
#' Try to convert back to numeric any numbers stored as factors, e.g., in a data.frame that did not use stringsAsFactors.
#' @details Uses as.numeric(as.character(x)) on the factor cols or vector,
#' but if there are both numbers and characters, it leaves it as factor, not numeric (which would put NA values in place of character elements).
#' NOTE: ** Not optimized for speed yet, so it is slow.
#' @param x Data.frame or vector, required. (If matrix, it is returned unaltered as a matrix).
#' @param stringsAsFactors Logical, TRUE by default, in which case a factor vector or col that has character elements,
#' and thus cannot be coerced to numeric without creating NA values, is left as a factor.
#' If FALSE, such a vector or col is converted to character class.
#' @return Returns a data.frame or vector, same shape as x (or matrix if given a matrix).
#' Any column that was integer or numeric is returned as numeric. \cr
#' Any character column or vector is returned as numeric if it could be coerced to numeric without creating any NA values because it has only numbers stored as text. \cr
#' Logical is returned as logical. \cr
#' When stringsAsFactors is TRUE, factor is returned as factor if it has any text that cannot be coerced to non-NA numeric.\cr
#' When stringsAsFactors is FALSE, factor is returned as character if it has any text that cannot be coerced to non-NA numeric.\cr
#' @seealso [as.vector()], [factor()], [data.table::data.table()], [matrix()]
#' @examples
#' a=factor(c(2,3,5)); b=factor(c('2', '3', '5')); c=factor(c('two','three','five'))
#' d=factor(c(2,'3','5')); e=factor(c(2,'three','five')); f=factor(c('2','three','5'))
#' g=factor(c(2,'3','five')); h=factor(c(NA, 3, 'five')); i=1:3;
#' j=rep('nonfactor',3); k=c(1,2,'text'); l=c(TRUE, FALSE, TRUE); m=c('2','3','5')
#' x=data.frame(a,b,c,d,e,f,g,h,i,j,k,l,m, stringsAsFactors=FALSE)
#' cat('\n')
#' cat('\n'); x; cat('\n'); cat('\n')
#' z=factor.as.numeric(x)
#' cat('\n'); z
#' cat('\n'); str(x)
#' cat('\n'); str(z);
#' cat('\n'); str( factor.as.numeric(x, stringsAsFactors=FALSE) )
#' for (i in 1:length(x)) {out<-factor.as.numeric(x[,i]);cat(class(out), out,'\n') }
#' for (i in 1:length(x)) {
#'   out<-factor.as.numeric(x[,i], stringsAsFactors = FALSE)
#'   cat(class(out), out,'\n')
#' }
#' @export
factor.as.numeric <- function(x, stringsAsFactors=TRUE) {

  if (is.matrix(x)) {
    return(x)
  }

  # simple "methods" to handle data.frame vs vector

  if (is.data.frame(x)) {

    z <- suppressWarnings( apply(as.matrix(x), 2, as.numeric) )
    z <- data.frame(z, stringsAsFactors = FALSE)

    if (stringsAsFactors) {
      colIsFactor <- sapply(x, is.factor)
      stringCells <- !is.na(x) & is.na(z)
      colHasStrings <- colSums(stringCells) > 0
      stayFactor <- colHasStrings & colIsFactor
      z[ , stayFactor] <- x[ , stayFactor]
      z[,!stayFactor][is.na(z[ , !stayFactor])] <- as.character( x[,!stayFactor][is.na(z[ , !stayFactor])] )
    } else {
      z[is.na(z)] <- as.character( x[is.na(z)] )
      # z[ is.na(z[,factcol] ), factcol  ] <- as.character( x[ is.na(z[,factcol] ), factcol  ] )
    }
    z[,sapply(x, is.logical)] <- x[,sapply(x, is.logical)]
    return(z)
  }

  if (!is.factor(x)) { return(x) }

  z <-  suppressWarnings( as.numeric(as.character(x)) )

  if (stringsAsFactors) {
    stringCells <- !is.na(x) & is.na(z)
    if (any(stringCells)) {
      return(x)
    } else {
      return(z)
    }
  } else {
    if (any(is.na(z))) {
      z[is.na(z)] <- as.character( x[is.na(z)] ) # ok whether it was NA or character in x, this should work
    }
    return(z)
  }
}

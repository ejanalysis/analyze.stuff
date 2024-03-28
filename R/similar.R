#' @title See how closely numeric values match in 2 datasets
#'
#' @description
#' Compare two vectors, matrices, or data.frames of numbers to see how often they are similar.
#'
#' @details
#' This function returns a matrix or vector showing how many rows in vector a are within 100-tol percent of the value in vector b.
#' May want to add a 3d case, where NA can match NA.
#' @param a Required first vector, data.frame, or matrix
#' @param b Required second vector, data.frame, or matrix
#' @param tol Number, 99.99 by default, specifying tolerance as a percentage 0-100, such that "similar" is defined as the two values being within 100-tol percent of each other.
#' @param na.rm Logical value, optional, FALSE by default. not implemented here yet. Should NA values be removed first, or compared and treated as NA matches NA.
#' @param shownames Logical value, optional, TRUE by default. Not used. Should names be shown in results?
#' @return Data.frame showing what # of rows are "similar" in dataset a vs b, for each column.
#' @seealso [similar.p()], [all.equal()], [identical()], [isTRUE()], [==()], [all()]
#' @examples
#'  similar.p(1:10, (1:10) * 1.001 )
#'  similar.p(data.frame(x=1:10, y=101:110), data.frame(other=1.001*(1:10),
#'    other2=c(101:109, 110.01) ))
#' @export
similar <- function(a,b,tol=99.99, na.rm=FALSE, shownames=TRUE) {

  # at first a and b had to be data.frames to have the length(a) etc. work right
  #  a <- as.data.frame(a)
  #  b <- as.data.frame(b)

  len <- ifelse( length(dim(a))==2, length(a), 1 )
  len.b <- ifelse( length(dim(b))==2, length(b), 1 )
  if (len!=len.b) {stop('a and b must be same shape and size')}
  out <- vector()

  # na.rm is not implemented here yet

  for (i in 1:len) {
    a.col <- if (length(dim(a))==2) {a.col <- a[ , i]} else {a.col <- a}
    b.col <- if (length(dim(b))==2) {b.col <- b[ , i]} else {b.col <- b}

    #print('len');print(len)
    #print('i');print(i)
    #print('length(dim(a))');print(length(dim(a)))
    #print('len a.col');print(length(a.col))
    #print('a');print(head(a,6))
    #print('a.col:'); print(head(a.col,6))

    #if (shownames) cat('\n')

    if (is.numeric( a.col ) && is.numeric(b.col)) {
      abspctdifs <- abs(ifelse(a.col + b.col==0, 0, (a.col - b.col) / ((a.col + b.col) / 2)))
      out[i] <- (sum( abspctdifs <  (100 - tol) / 100, na.rm=TRUE)); cat('  ')
      # don't say na.rm = na.rm passed from similar.p() or just a param here, since that would make %similar be reported as NA if any NA, instead of as 100% of non-NA values
    } else {
      out[i] <- NA
    }
  }
  out <- as.data.frame(out)
  if (len > 1) { rownames(out) <- names(a); colnames(out) <- 'similar.count'} else {rownames(out) <- rownames(a) }
  return(out)
}

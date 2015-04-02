#' @title See how closely numeric values match in 2 datasets
#'
#' @description
#' Compare two vectors, matrices, or data.frames of numbers to see how often they are similar.
#'
#' @details
#' This function returns a matrix or vector showing how many rows in vector a are within 100-tol percent of the value in vector b.
#' \cr May want to add a 3d case, where NA can match NA.
#' @param a Required first vector, data.frame, or matrix
#' @param b Required second vector, data.frame, or matrix
#' @param tol Number, 99.99 by default, specifying tolerance as a percentage 0-100, such that "similar" is defined as the two values being within 100-tol percent of each other.
#' @param na.rm Logical value, optional, FALSE by default. not implemented here yet. Should NA values be removed first, or compared and treated as NA matches NA.
#' @return Data.frame showing what % of rows are "similar" in dataset a vs b, for each column.
#' @seealso \code{\link{similar}}, \code{\link{all.equal}}, \code{\link{identical}}, \code{\link{isTRUE}}, \code{\link{==}}, \code{\link{all}}
#' @examples
#'  similar(1:10, (1:10) * 1.001 )
#'  similar(data.frame(x=1:10, y=101:110), data.frame(other=1.001*(1:10), other2=c(101:109, 110.01) )
#' @export
similar.p <- function(a,b,tol=99.99, na.rm=FALSE) {

  # na.rm is passed to similar()
  # May want to add a 3d case, where NA can match NA.

  # for now a and b must be data.frames (or vectors) to have the length(a) etc. work right
  if (is.matrix(a)) {a <- as.data.frame(a)}
  if (is.matrix(b)) {b <- as.data.frame(b)}

  len <- ifelse( length(dim(a))==2, length(a), 1 )
  len.b <- ifelse( length(dim(b))==2, length(b), 1 )
  if (len!=len.b) {stop('a and b must be same shape and size')}
  out <- vector()
  # can't we just call similar() and then divide results by each denominator?

  for (i in 1:len) {

    a.col <- if (length(dim(a))==2) {a.col <- a[ , i]} else {a.col <- a}
    b.col <- if (length(dim(b))==2) {b.col <- b[ , i]} else {b.col <- b}

    #cat('\n')

      if (is.numeric(a.col) && is.numeric(b.col)) {

        #print('length a.col'); print(length(a.col))
        #print('other denom'); print(sum(!is.na(a.col + b.col)))
        #print(head(a.col))
        #print(similar(as.vector(a.col), as.vector(b.col), tol, shownames=FALSE) )

        if (na.rm) {denom <- sum(!is.na(a.col + b.col)) } else {denom <- length(a.col)}
        out[i] <- 100 * similar(a.col, b.col, tol, shownames=FALSE, na.rm=na.rm) / denom # added na.rm here to pass to similar()
        } else {
        out[i] <- NA # cat('non numeric')
             }
  }
out <- as.data.frame(out)
out<-t(out)
if (len > 1) {rownames(out) <- names(a); colnames(out) <- 'pct.similar'} else {rownames(out) <- rownames(a)}
return(out)
}

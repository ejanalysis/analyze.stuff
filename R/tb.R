#' wrapper for table() that sorts by counts, decreasing
#'
#' @param x required, passed to table(x)
#' @param useNA default is 'always', passed to table()
#' @param ... other parameters passed to table() -- cannot pass anything to cbind or sort like decreasing=FALSE
#'
#' @return like cbind
#' @export
#' 
tb <- function(x, useNA = 'always', ...) {
  result <- cbind(sort(table(x, useNA=useNA, ...), decreasing=TRUE)) 
  names(result) <- 'count'
  return(result)
  # ignores anything passed to sort or cbind, including decreasing=FALSE
}

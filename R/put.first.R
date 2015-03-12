#' @title Simple way to put certain cols first, in a data.frame
#'
#' @description
#' Returns a data.frame with specified columns put first, before the others.
#' 
#' @param x Required data.frame that will have its columns reordered
#' @param fields required character vector of strings that are among the elements of names(x)
#' @return Returns a transformed data.frame with cols in new order
#' @seealso \code{\link{change.fieldnames}}
#' @examples
#' #
#' @export
put.first <- function(x, fields) {
  if (missing(x) | missing(fields)) {stop('must specify x=data.frame and fields=vector of fieldnames to move to first few columns of x')}
  if (!is.data.frame(x)) {stop('x must be a data.frame')}
  if (any(!(fields %in% names(x)))) {stop('fields must all be in names(x)')}
  return(x <- x[ , c(fields, names(x[!(names(x) %in% fields)]))])
}  

#' @title Differences between sets a and b
#'
#' @description
#' Returns the elements that in a or b but not in both (i.e., the differences between sets a and b)
#'
#' @param a Required vector
#' @param b Required vector
#' @return Vector of elements
#' @seealso \code{\link{setdiff}} which is a bit different
#' @examples
#' setdiff2(1:10, 3:12)
#' setdiff2(c('a','b','c'), c('b','c','d'))
#' @export
setdiff2 <- function(a,b) {
  union(a,b)[!( union(a,b) %in% intersect(a,b) )]
}

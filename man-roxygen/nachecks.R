#' @examples
#' \dontrun{
#' system.time(x= na.check(data.frame(a=-1:1e6, b='text', c=c(NA, 1, 2)), min.text=FALSE) )
#' system.time(x= na.check2(data.frame(a=-1:1e6, b='text', c=c(NA, 1, 2)), min.text=TRUE) )
#' na.check(data.frame(a=-1:10, b='text', c=c(NA, 1, 2)))
#' na.check2(data.frame(a=-1:10, b='text', c=c(NA, 1, 2)))
#' }
#' @seealso \code{signTabulate} in \pkg{matrixStats} \code{\link{minNonzero}} and experimental variations on na.check: \code{\link{na.check}} \code{\link{na.check2}}

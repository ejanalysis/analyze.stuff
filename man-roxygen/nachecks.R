#' @examples
#' print(Sys.time()); x= na.check(data.frame(a=-1:1e6, b='text', c=c(NA, 1, 2)), min.text=FALSE); print(Sys.time())
#' print(Sys.time()); x= na.check(data.frame(a=-1:1e6, b='text', c=c(NA, 1, 2)), min.text=TRUE); print(Sys.time())
#' na.check(data.frame(a=-1:10, b='text', c=c(NA, 1, 2)))
#' na.check1(data.frame(a=-1:10, b='text', c=c(NA, 1, 2)))
#' na.check2(data.frame(a=-1:10, b='text', c=c(NA, 1, 2)))
#' na.check3(data.frame(a=-1:10, b='text', c=c(NA, 1, 2)))
#' @seealso \code{\link{matrixStats::signTabulate}} \code{\link{minNonzero}} and experimental variations on na.check: \code{\link{na.check}} \code{\link{na.check1}} \code{\link{na.check2}} \code{\link{na.check3}}


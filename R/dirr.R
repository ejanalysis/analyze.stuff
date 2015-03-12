#' @title Directory listing of R-related files/folders
#' @description Function to let you see directory listing of files/folders ending in r, R, or RData
#' @param path A file path string, optional, default is current working directory.
#' @param ignore.case Logical, TRUE by default, optional. If FALSE, then this is case-sensitive.
#' @param ... Optional other parameters passed to \code{\link{dir}}
#' @return A directory listing.
#' @seealso \code{\link{dir2}} \code{\link{dirdirs}}
#' @export
dirr <- function(path='.', ignore.case=TRUE, ...) {
  dir(path=path, pattern='(\\.r|\\.R|\\.RData)$', ignore.case=ignore.case, ...)
}

#' @title Directory listing of R-related files/folders
#' @description Function to let you see directory listing of files/folders ending in r, R, or RData
#' @param path Path as character string, optional. Default is current working directory.
#' @param recursive Logical value, optional, FALSE by default. Should subdirectories be shown.
#' @param ... Optional other parameters passed to \code{\link{list.dirs}}
#' @return A directory listing
#' @seealso \code{\link{dir2}} \code{\link{dirr}}
#' @examples
#' dirdirs()
#' @export
dirdirs <- function(path='.', recursive=FALSE, ...) {
  cbind(list.dirs(path=path, recursive=recursive, ...) )
}

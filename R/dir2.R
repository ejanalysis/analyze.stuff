#' @title Directory listing using wildcard search
#' @description Function to let you see directory listing using wildcard search syntax like '*.R'
#' @param x Query string that can use wildcards to search directory
#' @param ignore.case Logical, TRUE by default, optional. If FALSE, then this is case-sensitive.
#' @param ... Optional other parameters passed to [dir()]
#' @return A directory listing.
#' @seealso [dirdirs()] [dirr()]
#' @examples
#' dir2('*.txt')
#' dir2('*.txt', path='~')
#' dir2()   # shows only files, not folders, if no x is specified.
#' dir2(path='~')  # shows only files, not folders, if no x is specified.
#'
#' @export
#'
dir2  <- function(x, ignore.case=TRUE, ...) {

 if (missing(x)) {x <- '*.*'}
  #if () { return( cbind(list.dirs(recursive=FALSE))) } # would show only directories not files
  return(cbind(dir(pattern = utils::glob2rx(x), ignore.case = ignore.case, ...)))
}


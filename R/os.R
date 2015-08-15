#' @title Windows, Mac, or other Unix?
#' @description Answers query about whether operating system is a certain type,
#'   or just reports type of operating system.
#' @param x Optional query, must be among these:
#'  'mac', 'apple', 'osx', 'darwin', 'win', 'windows', 'pc', 'microsoft', 'unix'
#' @return If queried, returns TRUE or FALSE (or NA if query not recognized).
#'   If no query, returns 'win', 'mac', or 'unix'
#' @seealso \code{\link{get.os}} which is a bit more limited
#' @examples
#'  os()
#'  if (os('mac')) {cat("Hi, I'm a Mac\n")} else {cat('I am not a Mac\n')}
#' @export
os <- function(x) {

  macronyms <- c('mac', 'apple', 'osx', 'darwin')
  winonyms <- c('win', 'windows', 'pc', 'microsoft')
  unixisms <- c('unix')

  OS.type <- .Platform$OS.type
  iswin <- OS.type=='win'
  ismac <- grepl('apple', R.version$platform)
  isunix <- OS.type=='unix'
  isotherunix <- isunix & (!ismac)

  if (missing(x)) {
    if (iswin) {return('win')}
    if (ismac) {return('mac')}
    if (isotherunix) {return('unix')}
    return(NA)
  }

  x <- tolower(x)
  if (x %in% macronyms) {return(ismac)}
  if (x %in% winonyms)  {return(iswin)}
  if (x %in% unixisms)  {return(isotherunix)}

  return(NA)
}

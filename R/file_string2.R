#' Read a text file and concatenate (some or all) lines by `\n`
#' Based on xfun::file_string()
#' @param file Path to a text file (should be encoded in UTF-8).
#' @param n Number of lines to view (or all, if n is missing/not specified)
#'
#' @return A character string of text lines concatenated by `\n`.
#' @export
#'
#' @examples
#'   # file_string2(system.file("DESCRIPTION", package = "xfun"))
#'   # help("file_string", package = "xfun")
file_string2 <- function(file, n) {
  # from xfun::file_string()
  x <- xfun::read_utf8(file)
  if (length(x)) {
    if (missing(n)) {n <- length(x)} else {n <- min(n, length(x))}
    x <- paste(x[1:n], collapse = "\n")
  }
  xfun::raw_string(x)
}

#' @title Specify Significant Digits for Each Column
#' @description
#'  Given a matrix or numeric data.frame, round each column to a specified column-specific number of significant digits.
#' @param dat Required, matrix or numeric data.frame with the values to be rounded.
#' @param digits Optional, 6 by default. Can be a vector as long as the number of columns in dat, where each elements specifies the
#'   number of significant digits to retain for numbers in the corresponding column of dat.
#' @return Returns dat, but with numbers rounded based on digits parameter.
#' @seealso \code{\link{signif}}
#' @examples
#'  signifarray(matrix(rnorm(9*5), ncol=5), 1:5)
#'  signifarray(data.frame(a=rnorm(10), b=rnorm(10), c=rnorm(10)), 1:3)
#' @export
signifarray <- function(dat, digits=6) {
  # for each column in dat, set to use digits specified as vector, one per column of dat

  # data.frame format just makes it easier to use something like mapply here to apply each sigfig setting to its own column of data
  if (!(is.data.frame(dat))) {dat <- as.data.frame(dat)}

#   if (missing(digits)) {
#     digits <- rep(digits, NCOL(dat)) # **** FIX SO THIS WORKS EVEN IF ARRAY/MATRIX/DATA.FRAME OF 1 COLUMN OR 1 ROW.
#   }

  y <- mapply(FUN=signif, x=dat, digits=digits)

  return(y)
}

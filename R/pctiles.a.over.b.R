#' @title Show the rounded values at 100 percentiles for a/b (or zero if b=0)
#'
#' @description
#' Get a quick look at a distribution by seeing the rounded values at 100 percentiles for a/b (setting a/b to zero if b=0)
#' @details
#' #  NOTE: THIS ONLY SHOWS PERCENTILES AND MEAN FOR THE VALID (NOT NA) VALUES !# Defining these types as type=1 and type="i/n" will create simple discontinuous quantiles, without interpolation where there are jumps in the values analyzed.
#' This is how  should be calculating percentiles as of 2/2013.
#' *** WARNING: Unless set type=1, the default type=7 in which case stats::quantile() FUNCTION INTERPOLATES, WHICH ISN'T OBVIOUS IN EVERY DATASET!
#' use type=1 to avoid interpolation.
#' and pctiles() rounded results so interpolation would be even less apparent.\cr
#' The quantile function will NOT interpolate between values if type=1:\cr
#' stats::quantile(1:12, probs=(1:10)/10, type=1)\cr
#'  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% \cr
#'  2    3    4    5    6    8    9   10   11   12 \cr
#'###########################\cr
#' **** IMPORTANT ***\cr
#'###########################\cr
#' *** WARNING: The Hmisc::wtd.quantile function DOES interpolate between values, even if type='i/n'\cr
#' There does not seem to be a way to fix that for the Hmisc::wtd.quantile() function.
#' For example, \cr
#' Hmisc::wtd.quantile(1:12, probs=(1:10)/10, type='i/n', weights=rep(1,12))\cr
#'  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% \cr
#'  1.2  2.4  3.6  4.8  6.0  7.2  8.4  9.6 10.8 12.0 \cr
#' @param a Required numeric vector of values that are numerator of ratio whose distribution you want to look at.
#' @param b Required numeric vector of values that are denominator of ratio whose distribution you want to look at.
#' @param digits Number, 3 by default, specifying how many decimal places to round to
#' @return Returns a data.frame
#' @seealso  \code{\link{pctiles}} \code{\link{pctiles.exact}} \code{\link{pctiles.a.over.b}} \code{\link{wtd.pctiles.exact}} \code{\link{wtd.pctiles}} \code{\link{wtd.pctiles.fast}}
#' @examples
#' #
#' @export
pctiles.a.over.b <- function(a, b, digits=3) {
  cbind(round(quantile(ifelse(b==0, 0, a / b), type=1, probs=(1:100) / 100, na.rm=TRUE), digits))
}

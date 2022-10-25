#' @title Show the values at 100 weighted percentiles
#'
#' @description
#' Get a quick look at a weighted distribution by seeing the 100 values that are the percentiles 1-100
#' @details
#' Provides weighted percentiles using wtd.quantile, see [Hmisc::wtd.Ecdf()] \cr \cr
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
#' @param x Required numeric vector of values whose distribution you want to look at.
#' @param wts NULL by default, or vector of numbers to use as weights in Hmisc::wtd.quantile
#' @param type 'i/n' is default. See help for [Hmisc::wtd.Ecdf()]
#' @param probs fractions 0-1, optional, (1:100)/100 by default, define quantiles to use
#' @param na.rm Logical optional TRUE by default, in which case NA values are removed first.
#' @return Returns a data.frame
#' @seealso  [pctiles()] [pctiles.exact()] [pctiles.a.over.b()] [wtd.pctiles.exact()] [wtd.pctiles()] [wtd.pctiles.fast()]
#' @export
wtd.pctiles.exact <- function(x, wts=NULL, na.rm=TRUE, type="i/n", probs=(1:100)/100) {

  # SHOW TABLE OF PERCENTILES, WEIGHTED, SO DISTRIBUTION OVER PEOPLE NOT PLACES
  # in a more convenient format

  #if (is.na(wts)) {wts <- rep(1, length(x))}

  cbind(Hmisc::wtd.quantile(x, wts, type=type, probs=probs, na.rm=na.rm))

  # could enhance this to take matrix or data.frame as input to have a vectorized function that gets wtd.pctiles for each of several columns, always using the same wts column though.
  # one way is not really vectorized, but just packaged as sapply(x, FUN=Hmisc::wtd.quantile(x, wts, type="i/n", probs=(1:100)/100, na.rm=na.rm) ) )
  # not too slow in terms of overhead if # of columns is limited, which it normally would be.
}

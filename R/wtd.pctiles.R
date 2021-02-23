#' @title Show the rounded values at 100 weighted percentiles
#'
#' @description
#' Get a quick look at a weighted distribution by seeing the 100 values that are the weighted percentiles 1-100
#' @details
#' Provides weighted percentiles using \code{\link[Hmisc]{wtd.stats}} \cr \cr
#' #  NOTE: THIS ONLY SHOWS PERCENTILES AND MEAN FOR THE VALID (NOT NA) VALUES !
#' Defining these types as type=1 and type="i/n" will create simple discontinuous quantiles, without interpolation where there are jumps in the values analyzed.
#' *** WARNING: Unless set type=1, the default type=7 in which case quantile() FUNCTION INTERPOLATES, WHICH ISN'T OBVIOUS IN EVERY DATASET!
#' use type=1 to avoid interpolation.
#' and pctiles() rounded results so interpolation would be even less apparent.\cr
#' The quantile function will NOT interpolate between values if type=1:\cr
#' quantile(1:12, probs=(1:10)/10, type=1)\cr
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
#' @param x Required, numeric vector (or data.frame) of values whose distribution(s) you want to look at.
#' @param wts NULL by default, or vector of numbers (same length as x vector or as a column of x) to use as weights in Hmisc::wtd.quantile
#' @param na.rm Logical optional TRUE by default, in which case NA values are removed first.
#' @param type 'i/n' is default. See help for wtd.quantile in \code{\link[Hmisc]{wtd.stats}}
#' @param probs fractions 0-1, optional, (1:100)/100 by default, define quantiles to use
#' @param digits Number, 3 by default, specifying how many decimal places to round to in results
#' @return Returns a data.frame, one row per probs, so 100 by default (1%ile through 100%ile), one col per col of x
#' @seealso  \code{\link{pctiles}} \code{\link{pctiles.exact}} \code{\link{pctiles.a.over.b}} \code{\link{wtd.pctiles.exact}} \code{\link{wtd.pctiles}} \code{\link{wtd.pctiles.fast}}
#' @export
wtd.pctiles <- function(x, wts=NULL, na.rm=TRUE, type="i/n", probs=(1:100)/100, digits=3) {
  #if (is.na(wts)) {wts <- rep(1, length(x))}
  if (NCOL(x) > 1) {
    # get the rownames right - sapply drops them
    results1 <- cbind(round(Hmisc::wtd.quantile(x[ , 1], wts, type=type, probs=probs, na.rm=na.rm), digits))
    if (NCOL(x) != 2) {
      results <- sapply(x[ , -1], function(z) wtd.pctiles(z, wts = wts, type=type, probs=probs, na.rm=na.rm, digits = digits), simplify = TRUE)
    } else {
      results <- cbind(round(Hmisc::wtd.quantile(x[ , 2], wts, type=type, probs=probs, na.rm=na.rm), digits))
    }
    results <- cbind(results1, results)
    colnames(results) <- colnames(x)
    return(results)
  } else {
    results <- cbind(round(Hmisc::wtd.quantile(x, wts, type=type, probs=probs, na.rm=na.rm), digits))
    if (!is.null(colnames(x)[1])) {colnames(results)[1] <- colnames(x)[1]}
    return(results)
  }
}

#' @title Overlay two simple histograms (pdf=probability density functions, not pdf filetype)
#' @description Overlay two simple histograms, for data below vs above a cutoff
#' @param x Variable for histogram
#' @param binx Variable that defines subsets
#' @param threshold Variable that defines cutoff, so binx<threshold is plotted first, in one color, then binx>=threshold in other color is drawn over that
#' @param colors Character vector length 2, with colors for first and second histogram
#' @param n Default is 100. n is just passed to the hist function.
#' @param ... Other named parameters sent to hist, such as main, xlab, ylab
#' @return Just plots using hist.
#' @examples
#'   \dontrun{
#'
#'  e <- bg$pm[!is.na(bg$pm)]
#'  dpct <- bg$pctmin
#'  dcount   <- bg$pop[!is.na(bg$pm)] *      dpct[!is.na(bg$pm)]
#'  refcount <- bg$pop[!is.na(bg$pm)] * (1 - dpct[!is.na(bg$pm)])
#'  brks <- 0:17
#'  etxt <- 'PM2.5'
#'  dtxt <- 'Minorities'
#'
#'  pop.cdf(        e, pcts = dpct, pops = bg$pop)
#'  pop.cdf2(       e, dcount, refcount, etxt, dtxt, brks)
#'  pop.cdf.density(e, dcount, refcount, etxt, dtxt )
#'
#'   pdf2(
#'   log10(bg$proximity.npl),
#'   bg$bin.EJ.DISPARITY.proximity.npl.eo, 10,
#'   main ='npl for high top 10 pct vs low EJ', xlab='NPL score log10'
#'   )
#'   }
#' @export
pdf2 <- function(x, binx, threshold, n=100, colors=c('gray', 'red'), ...) {
  hist(x[ binx < threshold], n, col = colors[1], freq = TRUE, ...)
  hist(x[ binx >= threshold], n, col = colors[2], freq = TRUE, add = TRUE, ...)
}



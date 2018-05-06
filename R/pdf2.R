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



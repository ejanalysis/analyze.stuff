#' @title Count columns with Value (at or) above (or below) Cutoff
#' @param x Data.frame or matrix of numbers to be compared to cutoff value.
#' @param cutoff numeric cutoff value to compare to
#' @param or.tied if TRUE, include ties (value in x equals cutoff)
#' @param na.rm if TRUE, used by colcounter to count only the non-NA columns in given row
#' @param below if TRUE, count x below cutoff not above cutoff
#' @param one.cut.per.col if FALSE, compare 1 cutoff to all of x.
#'   If TRUE, specify one cutoff per column.
#' @return vector of counts as long as NROW(x)
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct() tablefixed()
#' @export
#'
#' @examples
#'   \dontrun{
#'  pdata <- data.frame(a=rep(80,4),b=rep(93,4), col3=c(49,98,100,100))
#'   ### pdata <- EJAM::blockgroupstats[ , names_e_pctile]
#'   ## or ## pdata <- ejscreen::bg22[ , ejscreen::names.e.pctile]
#'  pcuts <-  5 * (0:20)  # <- as.vector(keystats_e['highcut', ])
#' colcounter_summary(        pdata, pcuts)
#' colcounter_summary_pct(    pdata, pcuts)
#' colcounter_summary_cum(    pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, 5 * (10:20))
#'
#' x80 <- colcounter(pdata, cutoff = 80, or.tied = T)
#' x95 <- colcounter(pdata, cutoff = 95, or.tied = T)
#' table(x95)
#' tablefixed(x95, NCOL(pdata))
#' cbind(at80=tablefixed(x80, NCOL(pdata)), at95=tablefixed(x95, NCOL(pdata)))
#'   }
#'
colcounter <- function(x, cutoff, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {
  # Function to count SCORES ABOVE BENCHMARK(S) at each place, returns list as long as NROW(x).
  #
  # *** This function should be very similar to or same as
  #    analyze.stuff::cols.above.count but or.tied= has different default?

  if (is.null(dim(x))) {numcols <- 1; stop('expected data.frame as x but has only 1 dimension')} else {numcols <- dim(x)[2]}
  if (missing(cutoff)) {
    if (one.cut.per.col) {
      cutoff <- colMeans(x, na.rm = na.rm)
    } else {
      cutoff <- rowMeans(x, na.rm = na.rm)
    }
  }
  if (one.cut.per.col) {
    if (length(cutoff) != NCOL(x)) {stop('length of cutoff should be same as number of columns in x if one.cut.per.col=T')}
    x <- t(as.matrix(x)) # this allows it to compare vector of N cutpoints to N columns
  } else {
    if (length(cutoff) != NROW(x) & length(cutoff) != 1) {stop('length of cutoff should be 1 or same as number of columns in x, if one.cut.per.col=F')}
  }
  if (below) {
    if  (or.tied) { y <- ( x <= cutoff) }
    if (!or.tied) { y <- ( x <  cutoff) }
  } else {
    if  (or.tied) { y <- ( x >= cutoff) }
    if (!or.tied) { y <- ( x >  cutoff) }
  }
  if (one.cut.per.col) {y <- t(y)}
  count.per.row <- rowSums(y, na.rm = na.rm)
  return(count.per.row)
}
######################################## #





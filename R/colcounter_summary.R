#' Summarize how many rows have N columns at or above (or below) various cutoffs?
#' @description Like colcounter() or cols.above.count()
#'   but will handle multiple cutoffs to compare to each indicator, etc.
#'   Table of counts, percents, cumulative counts, cumulative percents
#'   of places with N, or at least N, of the indicators
#'   >= benchmark(s)
#'
#' @param x Data.frame or matrix of numbers to be compared to cutoff value,
#'   like percentiles for example.
#' @param cutofflist vector of numeric cutoff values to compare to
#' @param or.tied if TRUE, include ties (value in x equals cutoff)
#' @param na.rm if TRUE, used by colcounter to count only the non-NA columns in given row
#' @param below if TRUE, count x below cutoff not above cutoff
#' @param one.cut.per.col if FALSE, compare each cutoff to all of x.
#'   If TRUE, specify one cutoff to use for each column.
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @seealso tablefixed()
#' @return A table of frequency counts
#' @export
#'
#' @examples
#'   \dontrun{
#'  pdata <- data.frame(a=rep(80,4),b=rep(93,4), col3=c(49,98,100,100))
#'   ### pdata <- EJAM::blockgroupstats[ , names_e_pctile]
#'   ## or ## pdata <- ejscreen::bg22plus[ , ejscreen::names.e.pctile]
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
colcounter_summary <- function(x, cutofflist, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {

  # Summarize frequencies - how many rows have N columns above various cutoffs?

  ## Examples
  ### pdata <- EJAM::blockgroupstats[ , names_e_pctile]
  ### pdata <- ejscreen::bg22plus[ , names.e.pctile]
  # pcuts <-  c(50,80,90,95,99)   # <- as.vector(keystats_e['highcut', ])
  #
  # x90 = colcounter(pdata, cutoff = 90)
  # x80 = colcounter(pdata, cutoff = 80)
  # table(x90)
  # tablefixed(x90)
  # table(x80)
  # tablefixed(x80)
  #
  # colcounter_summary(pdata, pcuts)
  # colcounter_summary(pdata, c(pcuts,10))


  # cutofflist must be a list where each element is length 1
  # or length = NCOL(x) if one.cut.per.col=T
  # AND each element of list is a scenario or threshold like percentile to check against all columns of x.
  ## if one.cut.per.col=F, cutoff vector is same length as columns in x, one each
  ## if one.cut.per.col=T, cutoff vector is several cutoffs to analyze in turn, but each cutoff is applied to all cols of x, and then next is.

  ccount <- NCOL(x)
  if (ccount == 1) x <- data.frame(x)
  countpersite_table <- sapply(
    cutofflist,
    FUN = function(thiscut) {
      tablefixed(
        colcounter(x, thiscut, or.tied = or.tied, na.rm = na.rm, below = below, one.cut.per.col = one.cut.per.col),
        ccount
      )
    }
  )
  colnames(countpersite_table) <-  cutofflist
  dimnames(countpersite_table) <- list(count.of.cols=rownames(countpersite_table), cutoff=cutofflist)
  return(countpersite_table)
}
######################################## #

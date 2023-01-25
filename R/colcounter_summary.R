#' Summarize how many rows have N columns at or above (or below) various cutoffs?
#' Like colcounter or cols.above.count
#'   but will handle multiple cutoffs to compare to each indicator, etc.
#'   Table of counts, percents, cumulative counts, cumulative percents
#'   of places with N, or at least N, of the indicators
#'   at or above the benchmark(s)
#' @param x Data.frame or matrix of numbers to be compared to cutoff value,
#'   like percentiles for example.
#' @param cutofflist vector of numeric cutoff values to compare to
#' @param or.tied if TRUE, include ties (value in x equals cutoff)
#' @param na.rm if TRUE, used by [colcounter()] to count only the non-NA columns in given row
#' @param below if TRUE, count x below cutoff not above cutoff
#' @param one.cut.per.col if FALSE, compare each cutoff to all of x.
#'   If TRUE, specify one cutoff to use for each column.
#' @seealso [colcounter_summary_all()] [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]
#' @seealso [tablefixed()]
#' @return A table of frequency counts
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
#' a3 <- colcounter_summary_all(    pdata, pcuts)
#'
#' x80 <- colcounter(pdata, cutoff = 80, or.tied = T)
#' x95 <- colcounter(pdata, cutoff = 95, or.tied = T)
#' table(x95)
#' tablefixed(x95, NCOL(pdata))
#' cbind(at80=tablefixed(x80, NCOL(pdata)), at95=tablefixed(x95, NCOL(pdata)))
#'   }
#'
colcounter_summary <- function(x, cutofflist, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {

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

#' Summarize how many rows have AT LEAST N columns at or above (or below) various cutoffs
#' See colcounter_summary() for more info and examples.
#' @param x Data.frame or matrix of numbers to be compared to cutoff value,
#'   like percentiles for example.
#' @param cutofflist vector of numeric cutoff values to compare to
#' @param or.tied if TRUE, include ties (value in x equals cutoff)
#' @param na.rm if TRUE, used by colcounter to count only the non-NA columns in given row
#' @param below if TRUE, count x below cutoff not above cutoff
#' @param one.cut.per.col if FALSE, compare each cutoff to all of x.
#'   If TRUE, specify one cutoff to use for each column.
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @return A table of cumulative frequency counts
#' @export
#'
colcounter_summary_cum <- function(x, cutofflist, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {
  apply(colcounter_summary(x, cutofflist = cutofflist, or.tied = or.tied, na.rm = na.rm,below = below,one.cut.per.col = one.cut.per.col),
        MARGIN = 2, FUN = function(thiscol) rev(cumsum(rev(thiscol))))
}

#' Summarize what percent of rows have N columns at or above (or below) various cutoffs
#' @details See examples for colcounter_summary_cum_pct()
#' @param x Data.frame or matrix of numbers to be compared to cutoff value,
#'   like percentiles for example.
#' @param cutofflist vector of numeric cutoff values to compare to
#' @param ... passed to colcounter_summary()
#'   like or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @export
#'
colcounter_summary_pct <- function(x, cutofflist, ...)  {
  100 * round(colcounter_summary(x, cutofflist = cutofflist, ...) / NROW(x), 2)
  }

#' Summarize what percent of rows have AT LEAST N columns at or above (or below) various cutoffs
#'
#' @param x Data.frame or matrix of numbers to be compared to cutoff value,
#'   like percentiles for example.
#' @param cutofflist vector of numeric cutoff values to compare to
#' @param ... passed to colcounter_summary_cum()
#'   like or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @export
#'
colcounter_summary_cum_pct <- function(x, cutofflist, ...) {
  100 * round(colcounter_summary_cum(x, cutofflist = cutofflist, ...) / NROW(x), 2)
  }



#' Summarize count (and percent) of rows with exactly (and at least) N cols >= various cutoffs
#' A wrapper for 4 functions: Returns four tables,
#'   using colcounter_summary(), colcounter_summary_pct(),
#'   colcounter_summary_cum(), colcounter_summary_cum_pct()
#' @param x Data.frame or matrix of numbers to be compared to cutoff value,
#'   like percentiles for example.
#' @param cutofflist vector of numeric cutoff values to compare to
#' @param ... passed to the 4 functions
#'   like or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @examples
#'     # df <- ejscreen::bg22[ , ejscreen::names.ej.pctile]
#'  df <- data.frame(a=rep(80,4),b=rep(93,4), col3=c(49,98,100,100))
#'  bench <- 5 * (0:20)
#'  a3 <- colcounter_summary_all(df, bench)
#'  a3[,'95',]
#'  a3[,,'cum_pct']
#'  a3['0',,]; a3[1,,]
#'  a3[dim(a3)[1],,]
#'  # a3['12',,]; a3[13,,]
#'
#'  barplot(colcounter_summary_cum_pct(pdata, pcuts)[ , '80'],
#'     ylab='% of places', xlab='# of indicators at/above cutoff',
#'     main='% of places with at least N/12 indicators >=80th percentile')
#'
#'  barplot(colcounter_summary(pdata, pcuts)[2:13 , '95'],
#'     ylab='# of places', xlab='# of indicators at/above cutoff',
#'     main='# of places with exactly N/12 indicators >=95th percentile')
#'
#'   # pdata <- ejscreen::bg22[ , ejscreen::names.e.pctile]
#'   colcounter_summary_cum_pct(pdata,c(50,80,90,95))
#'   xs <- 1:12
#'   plot(x=xs, y=colcounter_summary_cum_pct(pdata, 50)[xs+1], type='b', col='gray', ylim=c(0, 100),
#'     main='% of places with at least x/12 indicators >=Nth percentile', ylab='% of places', xlab='# of indicators')
#'   points(xs, colcounter_summary_cum_pct(pdata, 80)[xs+1], type='b', col='blue')
#'   points(xs, colcounter_summary_cum_pct(pdata, 90)[xs+1], type='b', col='orange')
#'   points(xs, colcounter_summary_cum_pct(pdata, 95)[xs+1], type='b', col='red')
#'   legend(x = 'topright', legend = paste0('>= ', c(50, 80, 90, 95),'th percentile'), fill = c('gray', 'blue', 'orange', 'red'))
#'
#'   # pdata <- ejscreen::bg22[ , ejscreen::names.ej.pctile]
#'   colcounter_summary_cum_pct(pdata,c(50,80,90,95))
#'   xs <- 1:12
#'   plot(x=xs, y=colcounter_summary_cum_pct(pdata, 50)[xs+1], type='b', col='gray', ylim=c(0, 40),
#'     main='% of places with at least x/12 indicators >=Nth percentile', ylab='% of places', xlab='# of indicators')
#'   points(xs, colcounter_summary_cum_pct(pdata, 80)[xs+1], type='b', col='blue')
#'   points(xs, colcounter_summary_cum_pct(pdata, 90)[xs+1], type='b', col='orange')
#'   points(xs, colcounter_summary_cum_pct(pdata, 95)[xs+1], type='b', col='red')
#'   legend(x = 'topright', legend = paste0('>= ', c(50, 80, 90, 95),'th percentile'), fill = c('gray', 'blue', 'orange', 'red'))
#'
#' @export
#'
colcounter_summary_all <- function(x, cutofflist, ...) {
  listall <- list(
    counts =  colcounter_summary(        x, cutofflist = cutofflist, ...),
    cum =     colcounter_summary_cum(    x, cutofflist = cutofflist, ...),
    pct =     colcounter_summary_pct(    x, cutofflist = cutofflist, ...),
    cum_pct = colcounter_summary_cum_pct(x, cutofflist = cutofflist, ...)
  )
  bincount <- length(0:NCOL(x))
  arrayall <- array(NA, dim=c(bincount, length(cutofflist), 4))
  for (i in 1:4) {arrayall[ ,, i] <- listall[[i]]}
  dimnames(arrayall) <- list(count=0:NCOL(x), cut=cutofflist, stat=c('count', 'cum', 'pct', 'cum_pct'))
  arrayall
}

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
#'
#' @param x passed to colcounter_summary()
#' @param ... passed to colcounter_summary()
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @export
#'
colcounter_summary_pct <- function(x, ...)  {100 * round(colcounter_summary(x, ...) / NROW(x), 2)}

#' Summarize what percent of rows have AT LEAST N columns at or above (or below) various cutoffs
#'
#' @param x passed to colcounter_summary_cum()
#' @param ... passed to colcounter_summary_cum()
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @export
#'
colcounter_summary_cum_pct <- function(x, ...) { 100 * round(colcounter_summary_cum(x, ...) / NROW(x), 2)}

#' Summarize count (and percent) of rows with exactly (and at least) N cols >= various cutoffs
#' A wrapper for 4 functions: Returns four tables,
#'   using colcounter_summary(), colcounter_summary_pct(),
#'   colcounter_summary_cum(), colcounter_summary_cum_pct()
#' @param x passed to the 4 functions
#' @param ... passed to the 4 functions
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct()
#' @export
#'
colcounter_summary_all <- function(x, ...) {
  list(
    counts =  colcounter_summary(x, ...),
    cum =     colcounter_summary_cum(x, ...),
    pct =     colcounter_summary_pct(x, ...),
    cum_pct = colcounter_summary_cum_pct(x, ...)
  )
}

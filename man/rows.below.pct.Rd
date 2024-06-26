% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rows.below.pct.R
\name{rows.below.pct}
\alias{rows.below.pct}
\title{Number or percent of rows (for each col) where value is below cutoff(s)}
\usage{
rows.below.pct(
  df,
  benchmarks = "mean",
  benchnames = "cutoff",
  na.rm = FALSE,
  or.tied = FALSE,
  below = FALSE,
  wts = 1,
  of.what = "all"
)
}
\arguments{
\item{df}{Data.frame or matrix, required.}

\item{benchmarks}{Default is 'mean' but otherwise this must be a number or numeric vector of thresholds to compare values to.}

\item{benchnames}{Default is 'cutoff' and this string is used to create colnames for the results}

\item{na.rm}{Logical value, optional, TRUE by default. Defines whether NA values should be removed first. Otherwise result will be NA when any NA is in a col.}

\item{or.tied}{Logical, FALSE by default, reporting on those < cutoff. But, if or.tied=TRUE, this reports on those <= cutoff.}

\item{below}{Logical, TRUE by default, which counts how many are below cutoff (or tied if or.tied). If FALSE, counts how many are above (or tied with) cutoff.}

\item{wts}{Number or vector, default is 1. Length must be a factor of number of rows in df, so \code{length(df[,1])} is an integer multiple of length(wts)  Applies weights to when counting how many.}

\item{of.what}{Optional, character, 'all' by default, defines xxx as the text used in "pct.above.xxx" (or below) for fieldnames in results}
}
\value{
Returns a vector of numbers of length equal to number of columns in df.
}
\description{
Alias for \code{\link[=pct.below]{pct.below()}}
}
\seealso{
\code{\link[=count.above]{count.above()}}  \code{\link[=pct.above]{pct.above()}} \code{\link[=pct.below]{pct.below()}} to see, for each column, the count or percent of rows that have values above or below a cutoff.

\code{\link[=cols.above.count]{cols.above.count()}} \code{\link[=cols.above.which]{cols.above.which()}} \code{\link[=cols.above.pct]{cols.above.pct()}} to see, for each row, the count or which or fraction of columns with numbers at/above/below cutoff.

\code{\link[=colcounter_summary]{colcounter_summary()}} \code{\link[=colcounter_summary_cum]{colcounter_summary_cum()}} \code{\link[=colcounter_summary_pct]{colcounter_summary_pct()}} \code{\link[=colcounter_summary_cum_pct]{colcounter_summary_cum_pct()}}  \code{\link[=tablefixed]{tablefixed()}}
}

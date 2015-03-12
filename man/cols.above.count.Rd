% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/cols.above.count.R
\name{cols.above.count}
\alias{cols.above.count}
\title{Number of Columns with Value at or above Cutoff}
\usage{
cols.above.count(x, cutoff, or.tied = FALSE, na.rm = TRUE, below = FALSE)
}
\arguments{
\item{x}{Data.frame or matrix of numbers to be compared to cutoff value.}

\item{cutoff}{The numeric threshold or cutoff to which numbers are compared. Default is arithmetic mean of row. Usually one number, but can be a vector of same length as number of rows, in which case each row can use a different cutoff.}

\item{or.tied}{Logical. Default is FALSE, which means we check if number in x is greater than the cutoff (>). If TRUE, check if greater than or equal (>=).}

\item{below}{Logical. Default is FALSE. If TRUE, uses > or >= cutoff. If FALSE, uses < or <= cutoff.}
}
\value{
Returns a vector the same size as the number of rows in x.
}
\description{
Find what number of columns have a value at or above some cutoff.
}
\details{
For a matrix with a few cols of related data, find what number of columns are at/above (or below) some cutoff.
Returns a vector of number indicating how many of the columns are at/above the cutoff.
Can be used in identifying places (rows) where some indicator(s) is/are at/above a cutoff, threshold value.
}
\note{
Future work: these functions could have wts, na.rm, &
allow cutoffs or benchmarks as a vector (not just 1 number), & have benchnames.
}
\examples{
out <- cols.above.count(x<-data.frame(a=1:10, b=rep(7,10), c=7:16), cutoff=7)
out
out # default is or.tied=FALSE
out <- cols.above.count(data.frame(a=1:10, b=rep(7,10), c=7:16),
  cutoff=7, or.tied=TRUE, below=TRUE)
out
out <- cols.above.count(data.frame(a=1:10, b=rep(7,10), c=7:16) )
 # Compares each number in each row to the row's mean.
out
}
\seealso{
\code{\link{count.above}} \code{\link{pct.above}} \code{\link{pct.below}} to see, for each column, the count or percent of rows that have values above or below a cutoff.

\code{\link{cols.above.count}} \code{\link{cols.above.which}} \code{\link{cols.above.pct}} to see, for each row, the count or which or fraction of columns with numbers at/above/below cutoff.

Other functions for above and below: \code{\link{cols.above.pct}};
  \code{\link{cols.above.which}};
  \code{\link{count.above}}; \code{\link{pct.above}};
  \code{\link{pct.below}}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmean.R
\name{harmean}
\alias{harmean}
\title{Harmonic mean}
\usage{
harmean(x, na.rm = FALSE)
}
\arguments{
\item{x}{Vector of numbers, required.}

\item{na.rm}{Logical value, optional, FALSE by default. If FALSE, result is NA if any of the values in x is NA. If TRUE, remove the NA values first.}
}
\value{
Returns a single number
}
\description{
Returns the harmonic mean of a vector of numbers.
}
\details{
The harmonic mean is one type of average. It is the reciprocal of the arithmetic mean of the reciprocals.
See \url{http://en.wikipedia.org/wiki/Harmonic_mean} for many applications of the harmonic mean.
}
\examples{
harmean(c(1,2,4))
}
\seealso{
\code{\link[=geomean]{geomean()}}  \code{\link[=mean]{mean()}}  \code{\link[=rms]{rms()}}
}

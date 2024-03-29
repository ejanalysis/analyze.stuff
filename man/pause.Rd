% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pause.R
\name{pause}
\alias{pause}
\title{Pause and wait specified number of seconds}
\usage{
pause(seconds = 1)
}
\arguments{
\item{seconds}{Time in seconds. Optional, default is 1 second.}
}
\value{
No value is returned.
}
\description{
Do nothing until time is up. Pause for some reason, wait for a download, etc.
}
\details{
The word pause is easier to remember than Sys.sleep, and Sys.sleep does not work on all systems apparently.
}
\seealso{
\code{\link[=Sys.sleep]{Sys.sleep()}}
}

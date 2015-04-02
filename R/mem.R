#' @title See what is using up memory
#' @description
#' See a list of the largest objects in memory, and how much RAM they are using up
#' Uses \code{\link{object.size}}  to return info on memory consumption for largest n objects
#' @param n Numeric, default is 10. How many objects to show (e.g., top 10)
#' @return Results in printing a list of objects and their sizes
#' @examples
#' \dontrun{
#'  mem()
#' mem(15)
#'
#' # draw pie chart
#' pie(object.sizes(), main="Memory usage by object")
#'
#' # draw bar plot
#' barplot(object.sizes(),
#'         main="Memory usage by object", ylab="Bytes", xlab="Variable name",
#'         col=heat.colors(length(object.sizes())))
#'
#' # draw dot chart
#' dotchart(object.sizes(), main="Memory usage by object", xlab="Bytes")
#'
#' ###################################
#' # memory.size() and memory.limit() and object.sizes() comparison:
#' ###################################
#'
#' # memory.size() to print aggregate memory usage statistics
#'
#' print(paste('R is using', memory.size(), 'MB out of limit', memory.limit(), 'MB'))
#'
#' # object.sizes() to see memory total used by objects:
#'
#' # NOTE: THIS DOES NOT MATCH TOTAL GIVEN BY memory.size();
#' #  it is only about half as much in the case I tried:
#' sum(as.numeric(object.sizes()))
#' # same, in MEGABYTES:
#' unclass(sum(as.numeric(object.sizes())))/1e6
#' # print to console in table format
#' object.sizes()
#' #	see a list of the top few variables:
#' head(cbind(object.sizes()))
#' }
#' @export
mem <- function(n=10) {
  if (length(ls(envir=.GlobalEnv)) > 0) {
    object.sizes <- function() {
      return(round(
        rev(sort(sapply(ls(envir=.GlobalEnv), function (object.name)
        object.size(get(object.name)))))/1e6,
        3))
      # format(object.size(get(object.name)), units = "MB")
      # could be used now instead of round( /1e6,3).
    }
    cat(paste('R is using', memory.size(), 'MB out of limit', memory.limit(), 'MB\n'))
    cat("Objects using the most memory (in MB):\n")
    ( head(cbind(MB=object.sizes()),n) )

  } else {
    cat('Nothing found in memory using ls(envir=.GlobalEnv)\n')
  }
}

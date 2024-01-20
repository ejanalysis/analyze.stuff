#' @title See what is using up memory but just for 1 environment
#' @description
#' See a list of the largest objects in memory, and how much RAM they are using up
#' Uses [object.size()]  to return info on memory consumption for largest n objects
#' @param n Numeric, default is 10. How many objects to show (e.g., top 10)
#' @param env The environment to check, defaulting to .GlobalEnv but
#'   could be something like env = as.environment(3) or 3
#' @return   prints a list of objects and their sizes
#' @examples
#' \dontrun{
#' mem()
#' mem(15)
#' mem(env = 2)
#'
#' # draw bar plot
#' y = mem()
#' barplot(y,
#'         main="Memory usage by object", ylab="Bytes", xlab="Variable name",
#'         col=heat.colors(length(y)))
#'
#' # draw pie chart again
#' pie(mem(), main = "Memory usage by object")
#'
#'
#' ################################## #
#' # memory.size() and memory.limit() and object.sizes() comparison:
#' ################################## #
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
mem <- function(n=10, env = .GlobalEnv) {
  if (!(is.environment(env))) {
    if  (is.numeric(env) & length(env) == 1 & round(env, 0) == env & env > 0 & env < 11) {
      env <- as.environment(env)
    } else {
      warning('env = ', env, ' is not an environment so defaulting to .GlobalEnv'); env = .GlobalEnv
    }}
  cat('also see ?ls() with defaults for use inside a function. ')
  if (length(ls(name = env)) > 0) {

    object.sizes <- function(env = .GlobalEnv) {
      return(round(
        rev(sort(sapply(ls(name = env), function (object.name)
          object.size(get(object.name, envir = env) ))))/1e6,
        3))
      # format(object.size(get(object.name)), units = "MB")
      # could be used now instead of round( /1e6,3).
    }

    if(.Platform$OS.type == "windows") {
      cat(  "  memory.size() is no longer supported fyi\n")
      # cat(paste('R is using', memory.size(), 'MB out of limit', memory.limit(), 'MB\n'))
    }

    cat("Objects using the most memory (in MB):\n")
    print( head(cbind(MB=object.sizes(env = env)),n) )
    # browser()
    pie_all = object.sizes(env = env)
    pie_top = pie_all[pie_all > sum(pie_all) * 0.05]

    pie_small_shown = setdiff(pie_all, pie_top)
    if (length(pie_small_shown) > 3) {
      pie_small_shown <- pie_small_shown[1:3]
        pie_other <- setdiff( setdiff(pie_all, pie_top), pie_small_shown)
        pie_other = sum(pie_other)
        names(pie_other) <- "other"
    } else {
      pie_other = NULL
    }


    names(pie_top)[which.max(pie_top)] <- paste0(names(pie_top)[which.max(pie_top)] , " (", pie_top[which.max(pie_top)], " MB)")
    if (pie_other == 0){ pie_show = c(pie_top, pie_small_shown} else {
      pie_show = c(pie_top, pie_small_shown, pie_other)}
    pie(pie_show, main = "Memory usage within just the specified environment (not all memory)")
    # invisible( object.sizes(env = env))
    return("")
  } else {
    cat('Nothing found in memory using ls(name = ', env,')\n')
    return("")
  }
}


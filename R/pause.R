#' @title Pause and wait specified number of seconds
#' @description Do nothing until time is up. Pause for some reason, wait for a download, etc.
#' @details The word pause is easier to remember than Sys.sleep, and Sys.sleep does not work on all systems apparently.
#' @param seconds Time in seconds. Optional, default is 1 second.
#' @return No value is returned.
#' @seealso [Sys.sleep()]
#' @export
pause <- function(seconds=1) {
  x <- FALSE
  x <- try({Sys.sleep(seconds); TRUE}, silent=TRUE)
  if (class(x)=="try-error") {
    start=Sys.time()
    while (Sys.time() - start < seconds) { }
  }
  return(NULL)
}

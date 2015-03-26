#' @title Pause and wait specified number of seconds
#' @description Do nothing until time is up. Pause for some reason, wait for a download, etc.
#' @param seconds Time in seconds. Optional, default is 1 second.
#' @return No value is returned.
#' @export
pause <- function(seconds=1) {
  start=Sys.time()
  while (Sys.time() - start < seconds) { }
}

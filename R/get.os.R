#' @title Windows or Mac?
#'
#' @description This function returns a character string "win" or "mac" 
#' depending on which operating system is being used (that's all it does right now)
#' @return Returns "win" or "mac" currently.
#' @export
get.os <- function() {
  os <- 'other'
  if (Sys.info()["sysname"]=="Windows") {
   os <- "win"
 }
 if (Sys.info()["sysname"]!="Windows") {
   os <- "mac"
 }
 return(os)
}

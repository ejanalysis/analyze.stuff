#' @title Print names(data.frame) commented out for easy pasting into code
#'
#' @description
#' Uses cat() to print names of data.frame, but in a column with # before each.
#' Make it convenient to copy/paste into .R code as comments
#' 
#' @param x Data.frame, required
#' @return Prints results
#' @export
names2 <- function(x) {  
  cat(paste("#  ", names(x), "\n", sep=""))  	# printed with hash sign so can paste into R code 
}

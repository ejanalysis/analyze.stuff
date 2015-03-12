#' @title Basic info on each col of data.frame
#'
#' @description
#' Returns basic information on each field in a data.frame, like count of rows that are zero, negative, 
#' NA, infinite, etc.
#' \cr\cr
#' Slow - work in progress
#' Leaves out logical, complex?, character, etc. cols
#' this version fails to handle fields that are factor class!?
#' @param df Matrix or data.frame to examine. Cannot be a single vector currently.
#' @param min.text Logical, optional, defaults to FALSE. If TRUE, tries to find minimum of numbers stored as text? Slows it down.
#' @return Returns a vector of results, one per col of df
#' @examples
#' na.check2(data.frame(a=-1:10, b='text', c=c(NA, 1, 2)))
#' @export
na.check2 = function(df) {

  cols=names(df)
  
  myfun <- function(x) { 
    c(
      mycount <- length(x),
      myvalid <- sum(!is.na(x)),
      mycount - myvalid,
      round(100 * myvalid / mycount, 1),
      sum(x==0, na.rm=TRUE),
      sum(x < 0, na.rm=TRUE),
      sum(is.infinite(x), na.rm=TRUE),
      
      myblank <-  if (mode(x)=='character') {sum(!nzchar(x), na.rm=TRUE)} else {0},
      
      myblank <- sum(x=='', na.rm=TRUE),
      mynbna <- myvalid - myblank,
      round(100 * mynbna / mycount, 1),
      #sum(!is.na(unique(x)))  , # this was 3% slower
      ifelse(any(is.na(x)), length(unique(x)) - 1, length(unique(x))),
      ifelse(all(x==0) || all(is.na(x)), NA, min(x[!is.na(x) & x!=0]))
    )
  }
 
  results <- data.frame( sapply(df, FUN=myfun), stringsAsFactors=FALSE)
  names(results) <- c(
    'count', 
    'not.na', 
    'na', 
    'pct.not.na', 
    'zero', 
    'neg', 
    'inf', 
    'blank', 
    'not.blank.not.na', 
    'pct.nbna', 
    'unique.not.na',
    'min.nonzero'
  )
  
  return(results)
}
 

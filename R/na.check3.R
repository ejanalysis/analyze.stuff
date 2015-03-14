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
#' @return Returns a vector of results, one per col of df
#' @template nachecks
#' @export
na.check3 = function(df) {

  cols=names(df)
  #df <- as.matrix(df)

  myfun <- function(x) {
    c(
      length(x),
      sum(!is.na(x)),
      sum(is.na(x)),
      round(100 * sum(!is.na(x)) / length(x), 1),
      sum(x==0, na.rm=TRUE),
      sum(x < 0, na.rm=TRUE),
      sum(is.infinite(x)),
      sum(x=='', na.rm=TRUE),
      sum(x!='', na.rm=TRUE),
      round(100 * sum(x!='', na.rm=TRUE) / length(x), 1),
      sum(!is.na(unique(x))),
      ifelse(all(x==0) | all(is.na(x)), NA, min(x[!is.na(x) & x!=0]))
    )
  }

  results <- matrix( sapply(df[ , cols], FUN=myfun), nrow=length(cols), byrow=TRUE)
  rownames(results) <- cols
  colnames(results) <- c(
    'bcount',
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
    'min.nonzero')
  return(results)
}

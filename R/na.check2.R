#' @title Basic info on each col of data.frame - testing faster way, but returns text
#'
#' @description
#' Returns basic information on each field in a data.frame, like count of rows that are zero, negative,
#' NA, infinite, etc.
#'
#' Slow - work in progress
#' Leaves out logical, complex?, character, etc. cols
#' this version fails to handle fields that are factor class!?
#' @param df Matrix or data.frame to examine. Cannot be a single vector currently.
#' @return Returns a vector of results, one per col of df
#' @examples
#' \dontrun{
#' system.time(x = na.check(data.frame(a = -1:1e6, b = 'text', c = c(NA, 1, 2)), min.text = FALSE) )
#' system.time(x = na.check2(data.frame(a = -1:1e6, b = 'text', c = c(NA, 1, 2)), min.text = TRUE) )
#' na.check(data.frame(a = -1:10, b = 'text', c = c(NA, 1, 2)))
#' na.check2(data.frame(a = -1:10, b = 'text', c = c(NA, 1, 2)))
#' }
#' @seealso [matrixStats::signTabulate()] [minNonzero()] and
#'   experimental variations on na.check: [na.check()]  [na.check2()]
#'
#'   @export
#'
na.check2 = function(df) {

  cols = names(df)
  #df <- as.matrix(df)

  myfun <- function(x) {
    c(
      length(x),
      sum(!is.na(x)),
      sum(is.na(x)),
      round(100 * sum(!is.na(x)) / length(x), 1),
      sum(x == 0, na.rm = TRUE),
      sum(x < 0, na.rm = TRUE),
      sum(is.infinite(x)),
      sum(x == '', na.rm = TRUE),
      sum(x != '', na.rm = TRUE),
      round(100 * sum(x != '', na.rm = TRUE) / length(x), 1),
      sum(!is.na(unique(x))),
      ifelse(all(x == 0) | all(is.na(x)), NA, min(x[!is.na(x) & x != 0]))
    )
  }

  # FASTER THAN na.check(), but returns character fields

  results <- matrix( sapply(df[ , cols], FUN = myfun), nrow = length(cols), byrow = TRUE)
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

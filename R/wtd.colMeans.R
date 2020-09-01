#' @title Weighted Mean of each Column - WORK IN PROGRESS (NA HANDLING NOT YET TESTED)
#' @description
#'  Returns weighted mean of each column of a data.frame or matrix, based on specified weights, one weight per row.
#'  Relies on \code{\link{weighted.mean}} and unlike \code{\link{wtd.colMeans2}} it also uses \code{\link[data.table]{data.table}}
#' @details ** Not yet handling factor or character fields well. \cr \cr
#'  For a given column of data values, \cr
#'   If just some values are NA (but no wts are NA), and na.rm = TRUE as in default, \cr
#'    returns a weighted mean of all non-NA values. \cr
#'   If just some values are NA (but no wts are NA), and na.rm = FALSE, \cr
#'    returns NA. \cr
#'   If all values are NA (but no wts are NA), \cr
#'    returns NaN. \cr
#'   If any weights are NA, it behaves like stats::weighted.mean, so it \cr
#'    returns NA, \cr
#'    unless each value corresponding to a NA weight is also NA and thus removed. \cr
#'    \cr
#'   Note Hmisc::wtd.mean is not exactly same as stats::weighted.mean since na.rm defaults differ \cr
#'    Hmisc::wtd.mean(x, weights=NULL, normwt="ignored", na.rm = TRUE )  \cr
#'    Note na.rm defaults differ. \cr
#'    weighted.mean(x, w,            ...,              na.rm = FALSE)
#' @import data.table
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of rows
#' @param by Optional vector, default is none, that can provide a single column name (as character) or character vector of column names,
#'   specifying what to group by, producing the weighted mean within each group.
#'   See help for \code{\link[data.table]{data.table}}
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found.
#'   Otherwise result will be NA when any NA is in a vector.
#' @param dims dims=1 is default. Not used. integer: Which dimensions are regarded as 'rows' or 'columns' to sum over.
#'   For row, the sum or mean is over dimensions dims+1, ...; for col it is over dimensions 1:dims.
#' @return If \code{by} is not specified, returns a vector of numbers of length equal to number of columns in df.
#'   If \code{by} is specified, returns weighted mean for each column in each subset defined via \code{by}.
#' @examples
#'   # library(analyze.stuff)
#'   wtd.colMeans(data.frame(a = 1:4, b = c(NA, 2, 3, 4)))
#'   wtd.colMeans(data.frame(a = 1:4, b = c(NA, 2, 3, 4)),    wts = c(1,1,1,1))
#'   wtd.colMeans(data.frame(a = 1:4, b = c(NA, 2, 3, 4)),    wts = c(NA,1,1,1))
#'   wtd.colMeans(data.frame(a = 1:4, b = c(NA, 2, 3, 4)),    wts = c(1,NA,1,1))
#'   wtd.colMeans(data.frame(a = 1:4, b = c(NA, 2, NA, 4)),   wts = c(1,1,1,1))
#'   wtd.colMeans(data.frame(a = 1:4, b = c(NA, NA, NA, NA)), wts = c(1,1,1,1))
#'
#'   # tests of wtd.colMeans
#'
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4))))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(1,1,1,1)))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, NA, 4)),   wts = c(1,1,1,1)))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, NA, NA, NA)), wts = c(1,1,1,1)))
#'
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(NA,1,1,1)))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(1,NA,1,1)))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(1,NA,NA,NA)))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(NA,NA,NA,NA)))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, NA, NA, NA)), wts = c(NA,NA,NA,NA)))
#'
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)), na.rm = FALSE))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(1,1,1,1), na.rm = FALSE))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, NA, 4)),   wts = c(1,1,1,1), na.rm = FALSE))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, NA, NA, NA)), wts = c(1,1,1,1), na.rm = FALSE))
#'
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(NA,1,1,1), na.rm = FALSE))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(1,NA,1,1), na.rm = FALSE))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(1,NA,NA,NA), na.rm = FALSE))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, 2, 3, 4)),    wts = c(NA,NA,NA,NA), na.rm = FALSE))
#'   suppressWarnings(wtd.colMeans(data.frame(a = 1:4, someNA = c(NA, NA, NA, NA)), wts = c(NA,NA,NA,NA), na.rm = FALSE))
#'
#'
#'   n <- 1e6
#'   mydf <- data.frame(pop = 1000 + abs(rnorm(n, 1000, 200)), v1 = runif(n, 0, 1),
#'    v2 = rnorm(n, 100, 15),
#'    REGION = c('R1', 'R2', sample(c('R1', 'R2', 'R3'), n-2, replace = TRUE)),
#'    stringsAsFactors = FALSE)
#'    mydf$pop[mydf$REGION == 'R2'] <- 4 * mydf$pop[mydf$REGION == 'R2']
#'   mydf$v1[mydf$REGION == 'R2'] <- 4 * mydf$v1[mydf$REGION == 'R2']
#'   wtd.colMeans(mydf[ , 1:3])
#'   wtd.colMeans(mydf[ , 1:3], wts = mydf$pop)
#'   wtd.colMeans(mydf, by = 'REGION')
#'   # R HANGS/STUCK: # wtd.colMeans(mydf[1:100, 1:3], by = mydf$REGION, wts = mydf$pop)
#'   mydf2 <- data.frame(a = 1:3, b = c(1, 2, NA))
#'   wtd.colMeans(mydf2)
#'   wtd.colMeans(mydf2, na.rm = TRUE)
#' @export
wtd.colMeans <- function(x, wts, by, na.rm = TRUE, dims = 1) {

  # require(data.table)
  if (!missing(wts)) {
    if (any(is.na(wts))) {warning('If any wts are missing i.e. NA values, stats::weighted.mean returns NA, which is what this function does - use 0 as the weight to omit a given value')}
  }
  if (any(is.na(x))) {warning('For cols with NA values, not fully tested -- I think mean uses total number of rows (or sum of non-NA weights) as denominator, not just rows where the actual value is non-NA!')}
  myna.rm <- na.rm
  # if just a vector (single col from data.frame with drop=TRUE) then cannot use setDT() -- setDT saves RAM by not making a copy
  if (NCOL(x) == 1) {
    x <- data.table(x)
  } else {
    x <- data.table(x)
    #setDT(x)
  }
  if (missing(wts)) {
    result <- x[ , lapply(.SD, mean,               na.rm = myna.rm), by = by]
  } else {
    result <- x[ , lapply(.SD, weighted.mean, wts, na.rm = myna.rm), by = by]
  }
  #setDF(x) # tried to avoid altering what was passed to this function but this did not do that
  setDF(result)
  return(result)

  # suppressWarnings( ) might be useful there
  #  Examples work when just source function and use it
  #   but doesn't work within package
  #  something to do with requiring data.table package I think
  #
  # wtd.colMeans2() does this: (not by zone, and may be slower but doesn't need data.table package)
  # colMeans(x * t(wts), na.rm=na.rm) * colSums(!is.na(x)) / sum(wts, na.rm=na.rm)
  # another way:
  # x = mydata[, lapply(.SD, function(x, y = pop) {sum(y * x)/sum(y)} ), by = "REGION"]
  # *** Question:  see notes in wtd.rowMeans()
}

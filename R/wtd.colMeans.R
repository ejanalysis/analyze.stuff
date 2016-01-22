#' @title Weighted Mean of each Column - WORK IN PROGRESS (NA HANDLING NOT YET TESTED)
#' @description
#' Returns weighted mean of each column of a data.frame or matrix, based on specified weights, one weight per row.
#' Now based on \code{\link[data.table]{data.table}} unlike \code{\link{wtd.colMeans2}}
#' @details ** not yet handling factor or character fields well.
#'   For cols with NA values, mean uses total number of rows (or sum of non-NA weights) as denominator?***,
#'   not just rows where the actual value is non-NA!  \cr
#'   Note Hmisc::wtd.mean is not exactly same as stats::weighted.mean since na.rm defaults differ \cr
#'   Hmisc::wtd.mean(x, weights=NULL, normwt="ignored", na.rm = TRUE ) # Note na.rm defaults differ. \cr
#'   weighted.mean(x, w,            ...,              na.rm = FALSE)
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of rows
#' @param by Optional vector, default is none, that can provide a single column name (as character) or character vector of column names,
#'   specifying what to group by, producing the weighted mean within each group.
#'   See help for \code{\link[data.table]{data.table}}
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found.
#'   Otherwise result will be NA when any NA is in a vector.
#' @param dims dims=1 is default. Not used. integer: Which dimensions are regarded as 'rows' or 'columns' to sum over.
#'   For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
#' @return If \code{by} is not specified, returns a vector of numbers of length equal to number of columns in df.
#'   If \code{by} is specified, returns weighted mean for each column in each subset defined via \code{by}.
#' @examples
#'   n <- 1e6
#'   mydf <- data.frame(pop=1000 + abs(rnorm(n, 1000, 200)), v1= runif(n, 0, 1),
#'     v2= rnorm(n, 100, 15), REGION=c('R1','R2',sample(c('R1', 'R2', 'R3'), n-2, replace=TRUE)))
#'   mydf$pop[mydf$REGION=='R2'] <- 4 * mydf$pop[mydf$REGION=='R2']
#'   mydf$v1[mydf$REGION=='R2'] <- 4 * mydf$v1[mydf$REGION=='R2']
#'   wtd.colMeans(mydf)
#'   wtd.colMeans(mydf, wts=mydf$pop)
#'   wtd.colMeans(mydf, by=mydf$REGION)
#'   wtd.colMeans(mydf, by=mydf$REGION, wts=mydf$pop)
#'   mydf2 <- data.frame(a=1:3, b=c(1,2,NA))
#'   wtd.colMeans(mydf2)
#'   wtd.colMeans(mydf2, na.rm=TRUE)
#' @export
wtd.colMeans <- function(x, wts=rep(1, NROW(x)), by, na.rm = TRUE, dims = 1) {
  # require(data.table)
  if (any(is.na(x)) | any(is.na(wts))) {warning('For cols with NA values, not fully tested -- I think mean uses total number of rows (or sum of non-NA weights) as denominator, not just rows where the actual value is non-NA!')}
  myna.rm <- na.rm
  # if just a vector (single col from data.frame with drop=TRUE) then cannot use setDT() -- setDT saves RAM by not making a copy
  if (NCOL(x)==1) {
    x <- data.table(x)
  } else {
    x <- data.table(x)
    #setDT(x)
  }
  result <- x[ , lapply(.SD, weighted.mean, wts, na.rm=myna.rm  ), by=by ]
  setDF( result )
  #setDF(x) # tried to avoid altering what was passed to this function but this did not do that
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

#' @title Weighted Mean of each Column - WORK IN PROGRESS
#' @description
#' Returns weighted mean of each column of a data.frame or matrix, based on specified weights, one weight per row.
#' Now based on \code{\link{data.table::data.table}[data.table]} unlike \code{\link{wtd.colMeans2}}
#' @details For cols with NA values, mean uses total number of rows (or sum of non-NA weights) as denominator,
#' not just rows where the actual value is non-NA!
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to 1 which is unweighted, numeric vector of length equal to number of rows
#' @param by Optional vector, default is none, that can provide a single column name (as character) or character vector of column names,
#' specifying what to group by, producing the weighted mean within each group.
#' See help for \code{\link{data.table::data.table}[data.table]}
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found.
#'   Otherwise result will be NA when any NA is in a vector.
#' @param dims dims=1 is default. Not used. integer: Which dimensions are regarded as 'rows' or 'columns' to sum over.
#'   For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
#' @return If \code{by} is not specified, returns a vector of numbers of length equal to number of columns in df.
#' @examples
#'   n <- 1e6
#'   mydf <- data.frame(pop=1000 + abs(rnorm(n, 1000, 200)), v1= runif(n, 0, 1),
#'     v2= rnorm(n, 100, 15), REGION=c('R1','R2',sample(c('R1', 'R2', 'R3'), n-2, replace=TRUE)))
#'   mydf$pop[mydf$REGION=='R2'] <- 4 * mydf$pop[mydf$REGION=='R2']
#'   mydf$v1[mydf$REGION=='R2'] <- 4 * mydf$v1[mydf$REGION=='R2']
#'   wtd.colMeans(mydf)
#'   wtd.colMeans(mydf, wts=mydf$pop)
#'   wtd.colMeans(mydf, by='REGION')
#'   wtd.colMeans(mydf, by='REGION', wts=mydf$pop)
#'   mydf2 <- data.frame(a=1:3, b=c(1,2,NA))
#'   wtd.colMeans(mydf2)
#'   wtd.colMeans(mydf2, na.rm=TRUE)
#' @export
wtd.colMeans <- function(x, wts=rep(1,NROW(x)), by, na.rm = FALSE, dims = 1) {

  # if (any(is.na(x)) | any(is.na(wts))) {warning('For cols with NA values, mean uses total number of rows (or sum of non-NA weights) as denominator, not just rows where the actual value is non-NA!')}

  x <- data.frame(x, wts=wts, stringsAsFactors=FALSE)

  xt <- data.table(x)

  result <- suppressWarnings( xt[ , lapply(.SD, weighted.mean, wts, na.rm=na.rm  ), by=by ])

  # result <- suppressWarnings( xt[, lapply(.SD, function(a) {sum(a * wts, na.rm=na.rm) / sum(wts, na.rm=na.rm)} ), by = by, with=TRUE])

  # result <-    xt[ , colMeans(.SD * t(.SD[,wts]), na.rm=na.rm, dims=dims) * colSums(!is.na(.SD), na.rm=na.rm, dims=dims) / sum(.SD[,'wts'], na.rm=na.rm) ]


  #  Examples work when just source function and use it
  #
  #   but doesn't work within package
  #
  #  something to do with requiring data.table package I think
  #


  # print(result)
  # print(1:(NCOL(result) - 1))
  # return(result[  ,1:(NCOL(result) - 1) , with=FALSE])
  data.frame( result[  , wts:=NULL], stringsAsFactors=FALSE)

  # warning(' **** THIS IS WORK IN PROGRESS AS IS wtd.rowMeans() !!! Check how NA values are handled ****')

  # wtd.colMeans2() does this: (not by zone, and may be slower but doesn't need data.table package)
  # colMeans(x * t(wts), na.rm=na.rm) * colSums(!is.na(x)) / sum(wts, na.rm=na.rm)

  # for new wtdColMeans, switched to this:
  #   require(data.table)
  #   n=1e6
  #   mydf <- data.frame(pop=1000 + rnorm(n, 1000, 100), v1= runif(n, 0, 1), v2= rnorm(n, 100, 15), REGION=sample(c('R1', 'R2', 'R3'), n, replace=TRUE))
  #   mydata <- data.table::data.table(mydf)
  #   x = mydata[, lapply(.SD, function(x, y = pop) {sum(y * x)/sum(y)} ), by = "REGION"]
  #   x

  #colSums(!is.na(x)) instead of length(w) might use all rows of given col where value in that col is not NA
  # t(wtd.rowMeans(t(x), wts, na.rm=na.rm, dims=dims)) #problem: treats value as zero if any in row is NA? ? ?
  # this might not work right handling NA VALUES IN wts vs in x ???****
  # instead of length(w) might want length2(w, na.rm=na.rm) or just na.rm=TRUE ???

  # *** Question:  see notes in wtd.rowMeans()

  # x
  # an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
  #
  # na.rm
  # logical. Should missing values (including NaN) be omitted from the calculations?
  #
  # dims
  # integer: Which dimensions are regarded as 'rows' or 'columns' to sum over. For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
  #
}

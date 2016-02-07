#' @title Weighted Mean of each Column - WORK IN PROGRESS
#' @description
#' Returns weighted mean of each column of a data.frame or matrix, based on specified weights, one weight per row.
#' But also see \code{\link[data.table]{data.table}} used for \code{\link{wtd.colMeans}}
#'
#' @param x Data.frame or matrix, required.
#' @param wts Weights, optional, defaults to nothing i.e. unweighted, and if specified must be vector of weights recycled to be same length as NROW(x) # not the name of the weights field in data.frame x, as single character string, e.g., "weightcol"
#' @param by Optional vector, default is none, that can provide a single column name (as character) or character vector of column names,
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before result is found. Otherwise result will be NA when any NA is in a vector.
#' @param dims dims=1 is default. **Not used.** integer: Which dimensions are regarded as 'rows' or 'columns' to sum over. For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
#' @return Returns a vector of numbers of length equal to number of columns in df.
#' @template meansum
#' @export
wtd.colMeans2 <- function(x, wts, by, na.rm = FALSE, dims = 1) {

  warning(' **** THIS IS WORK IN PROGRESS AS IS wtd.rowMeans() !!! Does not work & also check how NA values are handled ****')

  if (!missing(wts)) {
    x <- data.frame(x, wtcolname=wts, stringsAsFactors=FALSE)
    wts <- 'wtcolname'
  }

  myfun <- function(x, weights, na.rm) {
    colMeans(x * t(weights), na.rm=na.rm, dims=dims) *
      colSums(!is.na(x), na.rm=na.rm, dims=dims) /
      sum(weights, na.rm=na.rm)
  }

  if (missing(by)) {
    if (missing(wts)){
      myfun(x, 1, na.rm)
    } else {
      print('here')
      return( myfun(x, weights=x[ , wts], na.rm=na.rm) )
    }
  } else {
    if (missing(wts)){
      aggregate(x, by=list(x[ , by]), FUN=function(y) myfun(y, rep(1, NROW(y) ), na.rm = na.rm))
    } else {
      aggregate(x, by=list(x[ , by]), FUN=function(y) myfun(y, y[ , wts], na.rm = na.rm))
    }
  }

  # aggregate(x, by=list(x[ , by]), FUN=myfun )

  # May want to switch to this:
  #   require(data.table)
  #   n=1e6
  #   mydf <- data.frame(pop=1000 + rnorm(n, 1000, 100), v1= runif(n, 0, 1), v2= rnorm(n, 100, 15), REGION=sample(c('R1', 'R2', 'R3'), n, replace=TRUE))
  #   mydata <- data.table(mydf)
  #   x = mydata[, lapply(.SD, function(x, y = pop) {sum(y * x)/sum(y)} ), by = "REGION"]
  #   x
  ###############################
#   # for fast rollup:  apply a function to every column, or some of them
#   # Also see slam::rollup
#
#   # convert to data.table format for speed:
#   # bg <- data.table(bg) # would make a copy
#   setDT(bg)
#   regions.sum  <- bg[, lapply(.SD, sum), by=REGION, .SDcols = c("pop","mins","lowinc")]
#   states.sum   <- bg[, lapply(.SD, sum), by=FIPS.ST, .SDcols = c("pop","mins","lowinc")]
#   counties.sum <- bg[, lapply(.SD, sum), by=FIPS.COUNTY, .SDcols = c("pop","mins","lowinc")]
#   tracts.sum   <- bg[, lapply(.SD, sum), by=FIPS.TRACT, .SDcols = c("pop","mins","lowinc")]
#
#   # change back to data.frame format:
#   setDF(regions.sum)
#   setDF(states.sum)
#   setDF(counties.sum)
#   setDF(tracts.sum)
#   setDF(bg)
#
#   ###############################
#
#   # careful:
#   # > bg.dt[ 'blah', .N, nomatch=0]
#   # [1] 0
#   # > bg.dt[ 'blah', .N]
#   # [1] 1
#
#
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

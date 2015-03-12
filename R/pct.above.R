#' @title Number or percent of rows (for each col) where value exceeds cutoff(s)
#'
#' @description
#' Count the number or percent of rows (for each col of a data.frame) where the value exceeds some specified cutoff(s)
#' @details
# If wts is population counts, for example, this gives the PERCENT of people (not rows) for whom value in df[,x] exceeds benchmark for each column x
#'
#' below=FALSE by default, reports on those above (or tied with, if or.tied) cutoff. But if below=TRUE, this reports on those below  (or tied with, if or.tied) cutoff.
#' \cr
#' If df (passed to the function) is a data.frame or matrix, the function returns a vector of length= length(df) or number of cols in matrix.
#' \cr If df is just a vector, it is treated like a 1-column data.frame, so the function returns a single value.
#' \cr
#' \cr If benchmarks (passed to the function) is a data.frame matching df in dimensions, each value is used as the cutoff for the corresponding cell in df.
#' \cr If benchmarks is a vector of length= length(df), each value in benchmarks is the cutoff for the corresponding column in df.
#' \cr If benchmarks is a shorter vector, it is recycled. (e.g., a vector of length 2 would use the first benchmark as the cutoff for all odd columns of df, the second for all even columns of df).
#' \cr If benchmarks is a single numeric value, it is used as the cutoff value in every comparison for all of df.
#' \cr If benchmarks is omitted, the default behavior is to use the arithmetic mean value a column of df as the cutoff for that column of df.
#' \cr
#' \cr If benchnames is omitted, the word "cutoff" is used by default (unless benchmarks is also omitted).
#' \cr If benchnames is specified but benchmarks is not, the benchmarks default to the column means, so benchnames is ignored and "mean" is used instead.
#' \cr
#' \cr If wts is omitted the default is 1 which means no weighting. Just row counts.
#' \cr If wts is a vector of length= length(df[,1]) then each row of df uses the corresponding weight and count is sum of wts not count of rows.
#' \cr If wts is shorter than that, it is recycled but # of rows in df must be an integer multiple of length(wts).
#' \cr \cr
#' NA values in df are not counted and are not in the numerator of pct.above() but the denominator of pct.above() is a count of all rows of df, not just the non-NA ones.
#' \cr \cr
#' These could be renamed  rows.above.count(), rows.above.pct(), rows.above.which()
#' \cr to follow convention of cols.above.count(), cols.above.pct(), cols.above.which()
#' \cr and same using below too, like rows.below.pct() etc.
#' \cr and *** should make param names consistent, like x not df, cutoff(s) not benchmarks?, or.tied not gte
#' \cr but *** cols versions and all should have wts, na.rm, benchmarks as vector not just 1 number, benchnames, params
#' \cr and ** should have a "below" version for each variant
#' @param df Data.frame or matrix, required.
#' @param benchmarks Default is 'mean' but otherwise this must be a number or numeric vector of thresholds to compare values to.
#' @param benchnames Default is 'cutoff' and this string is used to create colnames for the results, such as above.cutoff.for.field1
#' @param or.tied Logical, FALSE by default, reporting on those > cutoff. But, if or.tied=TRUE, this reports on those >= cutoff.
#' @param below Logical, FALSE by default, which counts how many are above cutoff (or tied if or.tied). If TRUE, counts how many are below (or tied with) cutoff.
#' @param wts Number or vector, default is 1. Length must be a factor of # rows in df, so length(df[,1]) is an integer multiple of length(wts)  Applies weights to when counting how many.
#' @param na.rm Logical value, optional, TRUE by default. Defines whether NA values should be removed before value is found. Otherwise result will be NA when any NA is in a col.
#' @return Returns a vector of numbers of length equal to number of columns in df.
#' @template abovebelow
#' @examples
#' x <- data.frame(a=1:20, b=10, c=c(1:9,100:110))
#' mywts <- c(rep(1,10), rep(2,10))
#' mybench <- c(3,100,10)
#' mynames <- c("HI","USavg","HealthStandard")
#'
#' count.above(x, 0, wts=mywts)
#' count.above(x, 100, wts=mywts)
#' count.above(x, 10, wts=mywts)
#' count.above(x, mybench, wts=mywts)
#' cbind(count= count.above(x, mybench, mynames, wts=mywts))
#' cbind(pct= pct.above(x, benchmarks=mybench, benchnames=mynames, wts=mywts) )
#' cbind(
#'   count= count.above(x, mybench, mynames, wts=mywts),
#'   pct= pct.above(x, benchmarks=mybench, benchnames=mynames, wts=mywts) )
#' cbind(stat= pct.above(as.matrix(x), mybench, mynames, wts=mywts) )
#' cbind(stat= pct.above(1:100, 98 , wts=mywts))  # If only a single vector is passed, not a data.frame "Warning: df is a vector... converting to data.frame"
#'
#' # to find how many PLACES are at/above the 95th population-weighted percentile (won't be exactly 5% of places, just 5% of people):
#' mybench2 <- sapply(x, function(z) wtd.quantile(z, mywts, probs=0.95, na.rm=TRUE))
#' count.above(x, benchmarks=mybench2, benchnames=paste('pop.95th.', names(x), sep=''), wts=1 )
#' # to find how many PLACES are at/above the MEDIAN pop-wtd place (won't be exactly half of places, just half of people):
#' mybench2 <- sapply(x, function(z) wtd.quantile(z, mywts, probs=0.50, na.rm=TRUE))
#' count.above(x, benchmarks=mybench2, benchnames=paste('pop.median.', names(x), sep=''), wts=1 )
#'
#' # to find how many PEOPLE are at/above the 95th percentile place (won't be exactly 5% of people, just 5% of places):
#' mybench2 <- sapply(x, function(z) quantile(z, probs=0.95, na.rm=TRUE))
#' count.above(x, benchmarks=mybench2, benchnames=paste('95th.', names(x), sep=''), wts=mywts )
#' # to find how many PEOPLE are at/above the MEDIAN place (won't be exactly 50% of people, just 50% of places):
#' mybench2 <- sapply(x, function(z) quantile(z, probs=0.50, na.rm=TRUE))
#' count.above(x, benchmarks=mybench2, benchnames=paste('median.', names(x), sep=''), wts=mywts )
#'
#' cbind( pct.above(1:100, wts=mywts) )  # does not recycle weights in this situation of a single vector argument
#' count.above(data.frame(a=c(1:10, NA)), 2, wts=mywts)   # does not work if NA values
#' cbind( pct.above(data.frame(a=c(1:10, NA)), 0 , wts=mywts)) # "Error: wts must be a vector whose length is a factor of # rows in df, so length(df[,1]) is an integer multiple of length(wts) "
#' pct.above(data.frame(a=c(NA, NA, NA)), 3, wts=mywts) # "Error - df is a single NA value or single column with only NA values"
#' count.above(x, c(3,1), wts=mywts) # 3,1 is recycled as 3,1,3 since x has 3 cols
#' pct.above(x, benchnames=mynames, wts=mywts)  # ignores names since default benchmarks are column means
pct.above <- function(df, benchmarks='mean', benchnames='cutoff', na.rm=FALSE, or.tied=FALSE, below=FALSE, wts=1, of.what='all') {

  if (is.matrix(df)) {df <- as.data.frame(df) } #warning("df is a matrix... converting to data.frame")
  if (is.vector(df)) {df <- as.data.frame(df) } #warning("df is a vector... converting to data.frame")
  if (length(df[,1])==0) {stop("Error - zero rows in df, so percents would be undefined.")}

  if (length(benchmarks)==1 && benchmarks=='mean') {
    # use the simple mean value as the benchmark, & set benchnames <- "mean"
    # but note that may replace user-defined names if they set benchnames but not benchmarks
    # Use wtd.mean as benchmark if (wts!=1):
    if (any(wts!=1)) {
      benchmarks <- sapply(df, FUN=function(x) weighted.mean(x, wts, na.rm=TRUE) ) # note this part ignores the na.rm settings passed to here
    } else {
      benchmarks <- colMeans(df, na.rm=TRUE)
    }
    benchnames <- "mean"
  }

  if (length(benchnames) > length(df)) {
    print("Warning: length of benchnames must be less than or equal to # of cols in df. Renaming benchnames to 'cutoff' ")
    benchnames <- 'cutoff'
  }

  counts.above <- count.above(df, benchmarks, benchnames, or.tied=or.tied, below=below, wts=wts)

  if (length(counts.above)==1 && is.na(counts.above)) {print("Error in counts.above"); return(NA)}

  recycles <- length(df[ , 1]) / length(wts) # ensure sum of wts is right if it was recycled

  # THIS HAS NOT BEEN TESTED:
  denominator <- ifelse(na.rm,    sum( rep(wts, recycles)[!is.na(counts.above)] , na.rm=TRUE)  ,  sum( rep(wts, recycles), na.rm=TRUE))

  results <- counts.above / denominator

  if (or.tied) {mytext <- 'above.or.tied.with.'} else {mytext <- 'above.'}
  mytext <- paste('pct.of', of.what, mytext, sep='.')
  if (below) {mytext <- gsub('above', 'below', mytext)}

  names( results) <- paste(mytext, benchnames, ".for.", names(df), sep="")
  return( results)
}

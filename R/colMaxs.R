#' @title Get the max value of each column of a data.frame or matrix
#'
#' @description
#' Returns maximum value of each column of a data.frame or matrix.
#'   This returns -Inf for a column of all NA values, and
#'   returns 1 for logical column with any TRUE values.
#' @param df data.frame or matrix
#' @param na.rm TRUE by default. Should NA values be removed first
#' @return vector of numbers with length equal to number of cols in df
#' @details
#' ** NOTE: The useful matrixStats package is the basis for versions of
#'   rowMins, rowMax, colMins, colMaxs functions in this package.
#' Source: Henrik Bengtsson (2015). matrixStats: Methods that Apply to Rows and Columns of a Matrix.
#' R package version 0.13.1-9000.
#'
#'  https://github.com/HenrikBengtsson/matrixStats
#'
#' Initially, separate functions were written here for those four functions,
#' and the versions here were more flexible and convenient for some purposes,
#' e.g., handling data.frames and different na.rm defaults, but the matrixStats versions are much faster (e.g., by 4x or more).
#' Ideally, this analyze.stuff package would be modified to just extend those functions by
#' providing them methods to handle data.frames, not just matrix class objects,
#' and perhaps provide new or different parameters or defaults, such as defaulting to na.rm=TRUE instead of FALSE,
#' and handling factor class columns in a data.frame.
#' That has not been done yet, so colMaxs() etc. refer to the slower more flexible ones,
#'   and the faster matrix-only ones are via matrixStats::colMaxs etc.
#'
#' ** NOTE: max() and min() and [matrixStats::colMaxs()] from  matrixStats  etc. default to na.rm=FALSE,
#' but this function defaults to na.rm=TRUE because that just seems more frequently useful.
#'
#' ** NOTE: min and max & this function will handle character elements
#' by coercing all others in the column to character, which can be confusing
#'  -- e.g., note that min(c(8,10,'txt')) returns '10' not '8' and max returns 'txt'
#'  (also see the help for  ?Comparison )
#'
#' If this worked just like max() and min(), cols that are factors would make this fail.
#' max or min of a factor fails, even if as.character() of the factor would return a valid numeric vector.
#' That isn't an issue with a matrix, but a data.frame might have numbers stored as factor.
#' To fix that, this uses factor.as.numeric
#' with parameters that try to convert character or factor columns to numeric.
#' Based on how [min()] and [max()] behave,
#' return Inf or -Inf if no non-missing arguments to min or max respectively.
#' To suppress that warning when using this function, use  suppressWarnings()
#'
#' @seealso  [factor.as.numeric()] [rowMaxs()]
#'   [rowMins()] [colMaxs()] [colMins()]
#'   [count.above()] [pct.above()] [pct.below()]
#'   [cols.above.which()] [cols.above.pct()]
#'
#' @examples
#' blah <- rbind(NA, data.frame(a=c(0, 0:8), b=c(0.1+(0:9)), c=c(1:10), d=c(rep(NA, 10)),
#'   e=TRUE, f=factor('factor'), g='words', stringsAsFactors=FALSE) )
#' cbind(blah, min=rowMins(blah), max=rowMaxs(blah))
#' ## note that colMaxs(blah) was failing if any of the columns were not numeric
#' ##  rbind(blah, min = analyze.stuff::colMins(blah), max= analyze.stuff::colMaxs(blah))
#'
#' blah <- blah[ , sapply(blah, function(x) is.numeric(x) | is.logical(x)) ]
#' cbind(blah, min=rowMins(blah), max=rowMaxs(blah),
#'   mean=rowMeans(blah, na.rm=TRUE), sum=rowSums(blah, na.rm=TRUE))
#' rbind(blah, min=colMins(blah), max=colMaxs(blah),
#'   mean=colMeans(blah, na.rm=TRUE), sum=colSums(blah, na.rm=TRUE))
#'   # ** Actually, matrixStats does this ~4x as quickly,
#'   # although no practical difference unless large dataset:
#'   n <- 1e7
#' t1=Sys.time(); x=analyze.stuff::colMaxs( cbind(a=1:n, b=2, c=3, d=4, e=5)); t2=Sys.time()
#' print(difftime(t2,t1))
#' t1=Sys.time(); x=  matrixStats::colMaxs( cbind(a=1:n, b=2, c=3, d=4, e=5)); t2=Sys.time()
#' print(difftime(t2,t1))
#' # Note the latter cannot handle a data.frame:
#' \dontrun{
#' # This would fail:
#' matrixStats::colMaxs(   data.frame(a=1:10, b=2))
#' # This works:
#' analyze.stuff::colMaxs( data.frame(a=1:10, b=2))
#' }
#'
#' @export
#'
colMaxs <- function(df, na.rm=TRUE) {

  answers <- vector(mode = 'numeric', length = NCOL(df))
  # numeric_ones <- sapply(df, is.numeric) # fails to do right thing if matrix.
  # numeric_ones <- apply(df, 2, is.numeric) # works if matrix or data.frame more or less, but does work right if some cols of data.frame are not numeric!
  numeric_ones <- var_is_numeric_ish_stuff(df)

  if (is.matrix(df)) {
    answers[numeric_ones]  <- matrixStats::colMaxs(df[ , numeric_ones, drop = FALSE], na.rm = na.rm)
    answers[!numeric_ones] <- apply(df[ , !numeric_ones, drop = FALSE], 2, function(x) {max(x, na.rm = na.rm)})
  } else {
    # converts factors storing numbers to numbers, and factors storing character strings to character, which max likes better
    df <- factor.as.numeric(df, stringsAsFactors = FALSE )
    df[ , numeric_ones] <- sapply(df[, numeric_ones, drop = FALSE], as.numeric) # but note this coerces logical like TRUE to numeric like 1 !!
    answers[numeric_ones]  <- matrixStats::colMaxs(as.matrix(df[ , numeric_ones, drop = FALSE]), na.rm = na.rm)
    answers[!numeric_ones] <- apply(df[ , !numeric_ones, drop = FALSE], 2, function(x) {max(x, na.rm = na.rm)})
  }
  return(answers)
}


#
# n <- 10^5
# testdf <- data.frame(a=1:n, b=sample(c(TRUE,FALSE),n,replace = T), c=sample(letters,n,replace = T), factor(sample(letters,n,replace = T)), stringsAsFactors = FALSE)
# testdf <- cbind(testdf, testdf)
# testdf <- cbind(testdf, testdf)
# testdf <- cbind(testdf, testdf)
# testdf <- cbind(testdf, testdf) # 64 columns
# testdf <- cbind(testdf, testdf) # 128 columns
# dim(testdf)

# numeric_ones <- var_is_numeric_ish_stuff(testdf)
#  library(analyze.stuff) # then source new version

# library(microbenchmark)
# microbenchmark(colMaxs(testdf))

# library(profvis) # requires latest R version

# profvis({print(colMaxs(testdf))})
#
# system.time(
#   print(
#     colMaxs(testdf)
#   ))
#
# system.time(
#   print(
#   analyze.stuff::colMaxs(testdf) # old version
# ))
#
# ######
# system.time( # takes 3x the time of matrixStats version, to do just the numeric cols
#   { apply(testdf[, numeric_ones, drop=FALSE], 2, function(x) {
#     max(x, na.rm=TRUE)
#   } )
#   })
# system.time(
#   print(
#     matrixStats::colMaxs(as.matrix(testdf[ , numeric_ones,drop=FALSE]))
#   ))

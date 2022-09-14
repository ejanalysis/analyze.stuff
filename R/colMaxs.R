#' @title Get the max value of each column of a data.frame or matrix
#'
#' @description
#' Returns maximum value of each column of a data.frame or matrix.
#' @param df data.frame or matrix
#' @param na.rm TRUE by default. Should NA values be removed first
#' @return vector of numbers with length equal to number of cols in df
#' @template maxmin
#' @export
colMaxs <- function(df, na.rm=TRUE) {
  answers <- vector(mode = 'numeric', length = NCOL(df))
  numeric_ones <- sapply(df, is.numeric)

  if (is.matrix(df)) {
    answers[numeric_ones]  <- matrixStats::colMaxs(df[ , numeric_ones,drop=FALSE])
    answers[!numeric_ones] <- apply(df[ , !numeric_ones,drop=FALSE], 2, function(x) {max(x, na.rm=na.rm)})
  } else {
    # converts factors storing numbers to numbers, and factors storing character strings to character, which max likes better
    df <- factor.as.numeric(df, stringsAsFactors = FALSE )
    answers[numeric_ones]  <- matrixStats::colMaxs(as.matrix(df[ , numeric_ones,drop=FALSE]))
    answers[!numeric_ones] <- apply(df[ , !numeric_ones,drop=FALSE], 2, function(x) {max(x, na.rm=na.rm)})
    return(answers)
  }
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

# numeric_ones <- sapply(testdf, is.numeric)
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

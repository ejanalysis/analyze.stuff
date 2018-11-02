#' @title Basic info on each col of data.frame
#'
#' @description
#' Returns basic information on each field in a data.frame, like count of rows that are zero, negative,
#' NA, infinite, etc.
#'
#' Slow - work in progress
#' Leaves out logical, complex?, character, etc. cols
#'
#' @param df Matrix or data.frame to examine. Cannot be a single vector currently.
#' @param min.text Logical, optional, defaults to FALSE. If TRUE, tries to find minimum of numbers stored as text? Slows it down.
#' @return Returns a vector of results, one per col of df
#' @template nachecks
#' @export
na.check <- function(df, min.text = FALSE) {
  if (is.vector(df)) {
    stop('cannot yet handle a single vector, only a data.frame or matrix')
  } # would need to adjust code for this case
  if (is.matrix(df)) {
    df <- as.data.frame(df)
  } # this probably slows it down ***
  if (!is.data.frame(df)) {
    stop('cannot yet handle anything but data.frame or matrix')
  }
  if (any(class(df) == 'tbl')) {
    df <-
      as.data.frame(df)
    warning('converted tbl to data.frame to analyze it')
  }
  # may NEED TO RECODE TO HANDLE VECTOR NOT JUST DATA.FRAME, SO WOULD USE sum() not colSums() etc.

  cols <-
    colnames(df)    # df <- as.matrix(df) # can't do that since cols can be different types

  count1 <- ifelse(is.vector(df), length(df), length(df[, 1]))

  count <- rep(count1, ifelse(is.vector(df), 1, length(df)))

  numeric.col <-
    sapply(df, mode) == 'numeric' # leaves out logical, complex?, character, etc. ##### TEST time, date,...

  #  require(matrixStats) # quick way to get counts of negative, NA, and zero values; and colMaxs()
  signtabs <- matrix(NA, nrow = 4, ncol = length(cols))
  signtabs[, numeric.col] <-
    sapply(df[, numeric.col], function(x)
      matrixStats::signTabulate(x)[1:4])
  #neg=  signtabs[1,]
  #zero= signtabs[2,]
  #na=   signtabs[4,]

  #na <- signtabs[4,]
  not.na <- colSums(!is.na(df))
  na <- count - not.na
  #not.na <- count - na

  pct.not.na <- round(100 * not.na / count, 1)

  zero <- signtabs[2, ]  #colSums(df==0, na.rm=TRUE)

  # neg <- colSums(df < 0, na.rm=TRUE) # this is very slow for a data.frame
  neg <-
    signtabs[1, ]   # sapply(df, function(x) signTabulate(x)[1] )

  inf <- sapply(df, function(x)
    sum(is.infinite(x), na.rm = TRUE))
  #inf <- sum(sapply(df, is.infinite), na.rm=TRUE)

  is.char.col <- sapply(df, mode) == 'character'

  blank <-
    rep(0, length(df)) # i.e. once per column, only works if data frame, not matrix or vector!

  if (sum(is.char.col, na.rm = TRUE) > 1)  {
    blank[is.char.col] <-
      colSums(df[, is.char.col] == '', na.rm = TRUE)
  }

  if (sum(is.char.col, na.rm = TRUE) == 1) {
    blank[is.char.col] <-
      sum(df[, is.char.col] == '', na.rm = TRUE)
  }

  not.blank.not.na <- not.na - blank

  pct.nbna <- round(100 * not.blank.not.na / count, 1)

  unique.not.na <-
    sapply(
      df,
      FUN = function(x)
        ifelse(any(is.na(x)), length(unique(x)) - 1, length(unique(x)))
    )

  # *** This is the really slow line, using up about 3/4 of the total time for the na.check function, and it is due to the colMins that uses sapply:
  #minNonzero <- ifelse(all(df==0) || all(is.na(df)), NA, colMins(df[df!=0], na.rm=TRUE)) # is slower
  minNOTZERO <-
    sapply(df, function(x)
      ifelse(
        (!min.text &
           mode(x) != 'numeric') ||
          all(x == 0) || all(is.na(x)),
        NA,
        min(x[!is.na(x) & x != 0])
      ))
  # ~best but would be faster and return #s if avoid character cols
  # or could try using...
  # min.nonzero.val <- minNonzero(df)

  max <-
    analyze.stuff::colMaxs(df, na.rm = TRUE)
  # analyze.stuff::colMaxs can handle character fields but matrixStats::colMaxs cannot


  results <- data.frame(
    count = count,
    not.na = not.na,
    na = na,
    pct.not.na = pct.not.na,
    zero = zero,
    neg = neg,
    inf = inf,
    blank = blank,
    not.blank.not.na = not.blank.not.na,
    pct.nbna = pct.nbna,
    unique.not.na = unique.not.na,
    min.nonzero = minNOTZERO,
    max = max
  )

  return(results)
}

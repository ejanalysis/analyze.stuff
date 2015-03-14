#' @title Basic info on each col of data.frame
#'
#' @description
#' Returns basic information on each field in a data.frame, like count of rows that are zero, negative,
#' NA, infinite, etc.
#' \cr\cr
#' Slow - work in progress
#' Leaves out logical, complex?, character, etc. cols
#' this version fails to handle fields that are factor class!
#' @param mydat Matrix or data.frame to examine. Cannot be a single vector currently.
#' @param min.text Logical, optional, defaults to FALSE. If TRUE, tries to find minimum of numbers stored as text? Slows it down.
#' @return Returns a vector of results, one per col of df
#' @template nachecks
#' @export
na.check <- function(mydat, min.text=FALSE) {
  if (is.vector(mydat)) {
    #stop('cannot yet handle a single vector, only a data.frame or matrix')
    mydat <- data.frame(mydat, seq=1:length(mydat)) # quick fix, just adds a useless column to make it a data.frame, for now.
    } # would need to adjust code for this case

  if (is.matrix(mydat)) {mydat <- as.data.frame(mydat)} # this probably slows it down ***
  if (!is.data.frame(mydat)) {stop('cannot yet handle anything but data.frame or matrix')}
  # NEED TO RECODE TO HANDLE VECTOR NOT JUST DATA.FRAME, SO WOULD USE sum() not colSums() etc.

  cols=names(mydat)
  mycount    <- sapply(mydat, length)
  numeric.col <- sapply(mydat, mode)=='numeric' # leaves out logical, complex?, character, etc.
  # if parameter specified min.text=TRUE, then min.nonzero will include min( character fields ) which shows lowest in alphabetical order, but that's usually not needed & much faster to skip those columns.

  #  if (min.text) {
  #    want.min.col <- rep(TRUE, length(cols))
  #  } else {
  #    want.min.col <- numeric.col
  #  }
  #  if (min.text) {
  #    min.nonzero <- sapply(mydat, function(x) ifelse( all(x==0) || all(is.na(x)), NA, min(x[!is.na(x) & x!=0]))) #, # ~best but would be faster and return #s if avoid character cols
  #  } else {
  #    min.nonzero <- rep(NA, length(cols))
  #    min.nonzero[want.min.col] <- sapply(mydat[ , want.min.col], function(x) ifelse(all(x==0) || all(is.na(x)), NA, min(x[!is.na(x) & x!=0]))) #, # ~best but would be faster and return #s if avoid character cols
  #  }
  # this is almost as fast as above and much simpler:
  #  min.nonzero.val <- sapply(mydat, function(x) ifelse( (!min.text & mode(x)!='numeric') || all(x==0) || all(is.na(x)), NA, min(x[!is.na(x) & x!=0]))) #, # ~best but would be faster and return #s if avoid character cols
  #  since then I defined a function minNonzero() that is pretty fast
  min.nonzero.val <- minNonzero(mydat)

  #  require(matrixStats) # quick way to get counts of negative, NA, and zero values
  signtabs <- matrix(NA, nrow=4, ncol=length(cols))
  signtabs[ , numeric.col] <- apply(mydat[ , numeric.col], 2, function(x) matrixStats::signTabulate(x)[1:4] )
  #neg=  signtabs[1,]
  #zero= signtabs[2,]
  #na=   signtabs[4,]

  data.frame(
     count= mycount,
     #not.na= myvalid <- mycount - signtabs[4,],
     not.na= myvalid <- sapply(mydat, function(x) sum(!is.na(x)) ), # this handles character cols while signtabs can't
     #na=    signtabs[4,],
     na= mycount - myvalid,
     pct.not.na= round(100 * myvalid / mycount, 1),
     zero= signtabs[2,],  #zero= sapply(mydat, function(x) sum(x==0, na.rm=TRUE) ),
     #     neg=	 sapply(mydat, function(x) sum(x < 0, na.rm=TRUE) ), # was old way
     neg= signtabs[1,],
     inf=	 sapply(mydat, function(x) sum(is.infinite(x), na.rm=TRUE) ),
     blank= myblank <-  sapply(mydat, function(x) if (mode(x)=='character') {sum(!nzchar(x), na.rm=TRUE)} else {0}),
     not.blank.not.na= mynbna <- myvalid - myblank,
     pct.nbna= round(100 * mynbna / mycount, 1),

     # slowest lines are minNonzero and unique.not.na
     unique.not.na= sapply(mydat, function(x) ifelse(any(is.na(x)), length(unique(x)) - 1, length(unique(x)))) ,
     min.nonzero=   min.nonzero.val,
    #max= colMaxs(mydat, na.rm=TRUE), # WOULD NEED TO HANDLE non-numeric cols
    row.names=cols) # that works only for a data.frame
 }



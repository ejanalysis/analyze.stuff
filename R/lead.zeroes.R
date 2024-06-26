#' @title Add leading zeroes as needed
#'
#' @description
#' Returns the vector that was supplied, but with leading zeroes added where needed
#'   to make all elements have right number of characters.
#'
#' @details
#' This function can be useful in working with Census data where FIPS codes are often used.
#' Moving data to and from a spreadsheet can remove leading zeroes that may be necessary for proper data management.
#' This can apply to e.g., FIPS code for a block, block group, tract, county, or state.
#' Note: Number of digits in FIPS codes, assuming leading zeroes are there:\cr
#' state		2	(2 cumulative)\cr
#' county	  3	(5 cum)\cr
#' tract		6	(11 cum) (note 11 digits is ambiguous if not sure leading zero is there)\cr
#' block group 	1	(12 cum) (note 12 digits is ambiguous if not sure leading zero is there)\cr
#' block 	  1	(13 cum)\cr
#' @param fips Character vector, which can be FIPS codes or other data. Required.
#' @param length.desired NOT USED ANYMORE. INFERRED.
#'   Was a single numeric value (recycled), or vector of numbers, required, specifying how many characters long each returned string should be.
#' @return Returns a vector of same length as input parameter, NA for NA input elements
#' @examples
#' lead.zeroes(c('234','01234','3', NA, 'TEXT'), 5)
#' @export
lead.zeroes <- function(fips, length.desired=NULL) {

  # Very simple way to do this just for county fips that should be 5 digits,
  #   fips <- substr ( as.numeric(fips) + 100000, 2,7)


  #	TRY TO CLEAN UP vector of FIPS AND INFER GEOGRAPHIC SCALE

  ## keepNA = FALSE means that nchar() returns the number 2 instead of NA, which makes this work right.

  fips[nchar(fips, keepNA = FALSE) == 0]	<- NA
  # 1 or 2 characters is state fips
  fips[nchar(fips, keepNA = FALSE) == 1]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 1])
  # 3 is bad
  fips[nchar(fips, keepNA = FALSE) == 3]	<- NA
  # 4 or 5 is county
  fips[nchar(fips, keepNA = FALSE) == 4]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 4])
  # 6-9 are bad
  fips[nchar(fips, keepNA = FALSE) == 6]	<- NA
  fips[nchar(fips, keepNA = FALSE) == 7]	<- NA
  fips[nchar(fips, keepNA = FALSE) == 8]	<- NA
  fips[nchar(fips, keepNA = FALSE) == 9]	<- NA
  # 10 or 11 is tract
  fips[nchar(fips, keepNA = FALSE) == 10]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 10])
  # 12 is blockgroup
  # 13 is bad
  fips[nchar(fips, keepNA = FALSE) == 13]	<- NA
  # 14-15 is block
  fips[nchar(fips, keepNA = FALSE) == 14]	<- paste0("0", fips[nchar(fips, keepNA = FALSE) == 14])
  fips[nchar(fips, keepNA = FALSE) >= 16]	<- NA

  # MAYBE should remove or set to NA when State or County code is invalid? another function can check for that.

  return(fips)


  # if (length(fips) == 0) return(NULL)
  # navalues <- which(is.na(fips))
  # fips <- as.character(fips)
  # # could trim whitespace?
  #  if ( (length(length.desired) > 1) & (length(fips) != length(length.desired))) {
  #   warning("lengths of input vectors don't match, recycling length.desired")
  #   if (length(length.desired) > length(fips)) stop('length.desired cannot have more elements than fips')
  #
  # }
  # # recycle length.desired to be as long as fips vector
  # length.desired <- recycled_vector(length.desired, length(fips))
  # if ( any(length.desired == 0 | length.desired >= 100) ) {stop("error: string lengths must be >0 & <100")}
  # if ( any(nchar(fips) > length.desired, na.rm = TRUE) ) {stop("error: some are longer than desired length")}
  #
  # # fips <- paste( paste( rep( rep("0", length(length.desired)), length.desired), collapse = ""), fips, sep = "")
  # # would not work vectorized/ multiple length.desired values.
  #
  # # or maybe this, but can't say length.desired[i] unless it has same length as fip & can't handle recycling also:
  # for (i in 1:length(fips)) {
  #   fips[i] <- paste( paste( rep("0", length.desired[i]), collapse=""), fips[i], sep="")
  #   }
  #
  # fips <- substr(fips, nchar(fips) - length.desired + 1, nchar(fips))
  #
  # fips[navalues] <- NA
  # return(fips)
}


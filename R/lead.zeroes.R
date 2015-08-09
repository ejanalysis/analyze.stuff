#' @title Add leading zeroes as needed
#'
#' @description
#' Returns the vector that was supplied, but with leading zeroes added where needed to make all elements have specified number of characters.
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
#' @param length.desired A single numeric value (recycled), or vector of numbers, required, specifying how many characters long each returned string should be.
#' @return Returns a vector of same length as input parameter
#' @examples
#' lead.zeroes(c('234','01234','3'), 5)
#' @export
lead.zeroes <- function(fips, length.desired) {
	fips <- as.character(fips)
	# might trim whitespace?
	if ( (length(length.desired) >1) & (length(fips) != length(length.desired))) {print("warning: #s of inputs don't match")}
	if ( any(length.desired==0 | length.desired>=100) ) {stop("error: string lengths must be >0 & <100")}
	if ( any(nchar(fips) > length.desired) ) {stop("error: some are longer than desired length")}

	fips <- paste( paste( rep( rep("0", length(length.desired)), length.desired), collapse=""), fips, sep="")
	# does that work vectorized?

	# or maybe this, but can't say length.desired[i] unless it has same length as fip & can't handle recycling also:
	# fips <- for (i in 1:length(fip)) { paste( paste( rep("0", length.desired[i]), collapse=""), fips[i], sep="") }

	fips <- substr(fips, nchar(fips) - length.desired + 1, nchar(fips))

	return(fips)
}

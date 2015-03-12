#' @title Length of a list with or without NA values
#'
#' @description
#' Replacement for length(). Finds count of items like length(), but if set na.rm=TRUE then it doesn't count the items that are NA
#'
#' @param x A vector, required.
#' @param na.rm Logical value, optional, FALSE by default. Should NA values be left out of the count?
#' @return Returns a single number.
#' @examples
#' length2(c(1,2,3,NA))
#' length2(c(1,2,3,NA), na.rm=TRUE)
#' @export
length2 <- function(x, na.rm=FALSE) {
	if(na.rm) 	{sum(!is.na(x))}
	else		{length(x)}
}

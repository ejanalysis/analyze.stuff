#' @title Intersperse the elements of a vector, mixing 2d half of the list in with the 1st half
#' @description
#' This function will take a vector and split it in half (it must have an even # of elements) and
#' then will intersperse the elements, so for example, if the vector's starting order is 1,2,3, 4,5,6
#' the function returns the vector ordered as 1,4, 2,5, 3,6
#' @details
#' This is useful for example in reformatting a data.frame of Census data where the first n fields are estimates
#' and the next n fields are margin of error values corresponding to those estimates.
#' This function applied to the field names can reorder them to pair each estimate followed by its MOE.
#' @param x A vector with an even number of elements, required, character or numeric works.
#' @return Returns a vector that contains all the elements of the original, but reordered.
#' @examples
#' mydf <- data.frame(e1=101:120, e2=102:121, e3=111:130,
#'   m1=(101:120)*0.01, m2=(102:121)*0.01, m3=(111:130)*0.01)
#' mydf
#' mydf <- mydf[ , intersperse(names(mydf))]
#' mydf
#' @export
intersperse <- function(x) {
	len <- length(x)
	if ( (len/2)!=floor(len/2) ) {print("ERROR:must have even number of elements in vector"); return(x)}
	x <- x[as.vector(matrix(1:len, nrow=2, byrow=TRUE)) ]
	return(x)
}

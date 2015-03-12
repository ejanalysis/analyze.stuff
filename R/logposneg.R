#' @title log10(x) if positive, 0 if 0, -log10(-x) if negative
#' @description
#' Function that transforms a vector of numbers x into log10(x) if positive, 0 if 0, -log10(-x) if negative, 
#' useful for graphing something on a log scale when it has negative values.
#' This log scale expands outward from zero in both directions.
#' @param x numeric vector, required
#' @return A numeric vector of same length as x
#' @export
logposneg <- function(x) {
  ifelse(x < 0, -log10(-x), ifelse(x > 0, log10(x), 0))
}

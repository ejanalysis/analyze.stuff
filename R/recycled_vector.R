#' make a vector longer by recycling
#'
#' @param short_vector the vector to start with
#' @param long_length_desired how long should the resulting vector be. must be at least the length of short_vector
#'
#' @return a vector of length long_length_desired, formed by recycling the short_vector
#' @export
#'
#' @examples 
#' recycled_vector(1:3, 4)
#' recycled_vector(1:3, 8)
#' recycled_vector(c('a','b'), 4)
#' recycled_vector(c('all', 41, '&', 14), 5)
recycled_vector <- function(short_vector, long_length_desired) {
  # slow code but it works
  if (length(long_length_desired) > 1) {stop('long_length_desired must be a single number not a vector')}
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if (!is.wholenumber(long_length_desired)) stop('long_length_desired must be a whole number')
  shortlen = length(short_vector)
  if (long_length_desired < shortlen) {stop('short_vector length cannot exceed long_length_desired')}
  if (long_length_desired == shortlen) {return(short_vector)}
  times_fits_entirely = long_length_desired %/% shortlen
  leftoverlen = long_length_desired %% shortlen
  recycled_entirefit_part = rep(short_vector, times_fits_entirely)
  if (leftoverlen == 0) {
    return(recycled_entirefit_part)
  } else {
    recycled_remainder_part = short_vector[1:leftoverlen] 
    return(c(recycled_entirefit_part, recycled_remainder_part))
  }
}

#' @title Counts for Intersect, Union, etc. for Two Sets
#' @description
#'  This is just a convenient way to compare two sets (vectors) that overlap,
#'  to count how many are in each set, how many are in a not b, in b not a, in both, etc.
#' @param a Required vector, such as list of FIPS character codes.
#' @param b Required vector
#' @param values Default is FALSE. If TRUE, output is logical data.frame with
#'   union of only the unique elements as rownames, indicating which of those meet each criterion.
#' @return Returns a data.frame of counts by default, formatted for viewing as a small table.
#'   If values = TRUE, returns a larger data.frame (see `values` parameter). See examples.
#' @seealso [setdiff2()], [dplyr::setops()], [plotrix::intersectDiagram()]
#' @examples
#'  overlaps( c('Selectric 251','Selectric 245'),
#'   c('Selectric 245','Selectric 255','Selectric 255'))
#'  overlaps( c('Selectric 251','Selectric 245'),
#'   c('Selectric 245','Selectric 255','Selectric 255'), values = TRUE)
#'  overlaps(state.abb[1:3], state.abb[3:4])
#'  colSums( overlaps(state.abb[1:3], state.abb[3:4], values = TRUE) )
#'  colSums( overlaps(state.abb[1:3], state.abb[c(3:4,4,4,4,4,4)], values = TRUE) )
#'  overlaps(state.abb[1:3], state.abb[c(3:4,4,4,4,4,4)])
#'  overlaps(state.abb[1:3], state.abb[3:4], values = TRUE)
#'  \dontrun{
#'   overlaps(ejanalysis::get.state.info()$ST, state.abb)
#'   data(fips.state, package='acs')
#'   overlaps(lead.zeroes(fips.state$STATE,2), ejanalysis::get.state.info()$FIPS.ST)
#'   data(fips.county, package='acs')
#'   overlaps(ejanalysis::get.county.info()$FIPS.COUNTY,
#'     paste(analyze.stuff::lead.zeroes(fips.county$State.ANSI,2),
#'     analyze.stuff::lead.zeroes(fips.county$County.ANSI,3), sep=''))
#'
#'   colSums( overlaps(ejanalysis::get.state.info()$ST, c(999, state.abb), values = TRUE) [ , 2:8])
#'
#'  }
#' @export
overlaps <- function(a, b, values = FALSE) {

  #     a and b, intersect(a,b) gives uniques
  #     a not b  setdiff(a,b) gives uniques
  #     b not a  setdiff(b,a) gives uniques
  #     a or b,  union(a,b) gives uniques

  a.in.b <- a %in% b
  b.in.a <- b %in% a

  total.in.a.only <- sum(!a.in.b)
  total.in.b.only <- sum(!b.in.a)

  unique.in.a.only <- length(setdiff(a,b)) # should be same as length( setdiff(a,b) )
  unique.in.b.only <- length(setdiff(b,a)) # should be same as length( setdiff(b,a) )

  results <- data.frame( rbind(
    unique = c(
      in.a =  length(unique(a)),
      in.b =  length(unique(b)),
      in.a.only = unique.in.a.only,
      in.b.only = unique.in.b.only,
      in.one.only = unique.in.a.only + unique.in.b.only,
      overlap = length(intersect(a,b)),
      union = length(union(a,b))
    ),
    total = c(
      in.a =  length(a),
      in.b =  length(b),
      in.a.only =   total.in.a.only,
      in.b.only =   total.in.b.only,
      in.one.only = total.in.a.only + total.in.b.only,
      overlap = sum(a.in.b) + sum(b.in.a),
      union = length(a) + length(b)
    )
  ), stringsAsFactors = FALSE)

  if (values) {

    # PROVIDE A LOGICAL data.frame OF ALL unique NAMES & WHERE EACH IS FOUND:

    x <- unique(c(a, b[!b.in.a])) # like unique(union(a,b)) but in a particular order, showing all unique a first.
    xina <- x %in% a
    xinb <- x %in% b
    results <- data.frame(
      #name = x,
      in.a =  xina,
      in.b =  xinb,
      in.a.only = xinaonly <- xina & !xinb,
      in.b.only = xinbonly <- xinb & !xina,
      in.one.only = xinaonly | xinbonly,
      overlap = (xina) & (xinb),
      union = TRUE,
      stringsAsFactors = FALSE)
    rownames(results) <- x
  }

  return(results)
}

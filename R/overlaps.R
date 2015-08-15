#' @title Counts for Intersect, Union, etc. for Two Sets
#' @description
#'  This is just a convenient way to compare two sets (vectors) that overlap,
#'  to count how many are in each set, how many are in a not b, in b not a, in both, etc.
#' @param a Required vector, such as list of FIPS character codes.
#' @param b Required vector
#' @return Returns a list of counts, formatted for viewing as a small table. See example.
#' @seealso \code{\link{setdiff2}}, \code{\link{sets}}, \code{\link[dplyr]{setops}}, \code{\link[plotrix]{intersectDiagram}}
#' @examples
#'  overlaps( c('Selectric 251','Selectric 245'), c('Selectric 245','Selectric 255','Selectric 255'))
#'  \donotrun{
#'           unique total
#'   in.a.only   1      1
#'   in.b.only   1      2
#'   overlap     1      2
#'   in.a        2      2
#'   in.b        2      3
#'   union       3      5
#'   in.one.only 2      3
#'
#'   overlaps(ejanalysis::get.state.info()$ST, state.abb)
#'   data(fips.state, package='acs')
#'   overlaps(lead.zeroes(fips.state$STATE,2), ejanalysis::get.state.info()$FIPS.ST)
#'   data(fips.county, package='acs')
#'   overlaps(ejanalysis::get.county.info()$FIPS.COUNTY,
#'     paste(analyze.stuff::lead.zeroes(fips.county$State.ANSI,2),
#'     analyze.stuff::lead.zeroes(fips.county$County.ANSI,3), sep=''))
#'  }
#' @export
overlaps <- function(a,b) {

  #     a and b, intersect(a,b) gives uniques
  #     a not b  setdiff(a,b) gives uniques
  #     b not a  setdiff(b,a) gives uniques
  #     a or b,  union(a,b) gives uniques

  a.in.b <- a %in% b
  b.in.a <- b %in% a

  total.in.a.only <- sum(!a.in.b)
  total.in.b.only <- sum(!b.in.a)

  unique.in.a.only <- length(setdiff(a,b))
  unique.in.b.only <- length(setdiff(b,a))

  return( cbind(
    unique=list(
      in.a.only= unique.in.a.only,
      in.b.only= unique.in.b.only,
      overlap= length(intersect(a,b)),
      in.a=length(unique(a)),
      in.b=length(unique(b)),
      union=length(union(a,b)),
      in.one.only= unique.in.a.only + unique.in.b.only
    ), total=list(
      in.a.only=   total.in.a.only,
      in.b.only=   total.in.b.only,
      overlap= sum(a.in.b) + sum(b.in.a),
      in.a=length(a),
      in.b=length(b),
      union=length(a) + length(b),
      in.one.only=   total.in.a.only + total.in.b.only
    )
  ))
}

#' @title Counts for Intersect, Union, etc. for Two Sets
#' @description
#'  This is just a convenient way to compare two sets (vectors) that overlap,
#'  to count how many are in each set, how many are in a not b, in b not a, in both, etc.
#' @param a Required vector, such as list of FIPS character codes.
#' @param b Required vector
#' @param ab_names optional vector of 2 names to use as labels if drawing Venn
#' @param ab_colors optional vector of 2 colors if drawing Venn
#' @param venn_draw optional, whether to plot the Venn in viewer window
#' @param venn_save optional, whether to save a png of the Venn plot
#' @param filename optional, name of png file to use if venn_save is TRUE
#' @param ... optional, passed to [VennDiagram::draw.pairwise.venn()]
#' @param values Default is FALSE. If TRUE, output is logical data.frame with
#'   union of only the unique elements as rownames, indicating which of those meet each criterion.
#' @param overlapcount if number is provided, a and b are interpreted as counts and
#'   overlapcount is interpreted as count of intersection, and
#'   parameter called values is ignored.
#' @return Returns a data.frame of counts by default, formatted for viewing as a small table.
#'   If values = TRUE, returns a larger data.frame (see `values` parameter).
#'   If overlapcount provided, returns nothing. See examples.
#' @seealso [setdiff2()], [dplyr::setops()], [plotrix::intersectDiagram()]
#' @examples
#'  overlaps(11022+48541, 8748+48541, overlapcount= 48541, ab_names = c(
#'  "59,563 block groups have\n any Suppl EJ index\n >=90th pctile in State",
#'  "57,289 block groups have\n any Suppl EJ index\n >=90th pctile in US"),
#'   ab_colors = c("lightblue", "yellow"))
#'
#'  overlaps( c('Selectric 251','Selectric 245'),
#'   c('Selectric 245','Selectric 255','Selectric 255'))
#'  overlaps( c('Selectric 251','Selectric 245'),
#'   c('Selectric 245','Selectric 255','Selectric 255'), values = TRUE)
#'  overlaps(state.abb[1:3], state.abb[3:4])
#'    colSums( overlaps(state.abb[1:3], state.abb[3:4], values = TRUE)[,-1] )
#'  colSums( overlaps(state.abb[1:3], state.abb[c(3:4,4,4,4,4,4)],
#'  values = TRUE)[,-1] )
#'  overlaps(state.abb[1:3], state.abb[c(3:4,4,4,4,4,4)])
#'  overlaps(state.abb[1:3], state.abb[3:4], values = TRUE)
#'  \dontrun{
#'   overlaps(ejanalysis::get.state.info()$ST, state.abb)
#'   data(fips.state, package='acs')
#'   overlaps(lead.zeroes(fips.state$STATE,2),
#'     ejanalysis::get.state.info()$FIPS.ST)
#'   data(fips.county, package='acs')
#'   overlaps(ejanalysis::get.county.info()$FIPS.COUNTY,
#'     paste(analyze.stuff::lead.zeroes(fips.county$State.ANSI,2),
#'     analyze.stuff::lead.zeroes(fips.county$County.ANSI,3), sep=''))
#'
#'   colSums( overlaps(ejanalysis::get.state.info()$ST, c(999, state.abb),
#'   values = TRUE)[ , 2:8])
#'
#'  }
#' @export
overlaps <- function(a, b, values = FALSE,
                     ab_names=c('a','b'), ab_colors=c('gray', 'orange'),
                     venn_draw=TRUE, venn_save=FALSE, filename='venn.png', overlapcount=NULL,   ...) {

  if (!is.null(overlapcount)) {
    if (venn_draw) {
      grid::grid.newpage()
      VennDiagram::draw.pairwise.venn(
        area1 = a,
        area2 = b,
        cross.area = overlapcount,
        category = ab_names,
        fill = ab_colors, ...
      )
    }
    if (venn_save) {
      myplot <- VennDiagram::draw.pairwise.venn(
        area1 = a,
        area2 = b,
        cross.area = overlapcount,
        category = ab_names,
        fill = ab_colors, ...
      )

      # tiff(
      #   filename = tempfile(
      #     pattern = 'venn',
      #     fileext = '.tiff'
      #   ),
      #   compression = "lzw")
      # grid.draw(myplot)
      # dev.off()


      png(filename = filename)
      grid::grid.draw(myplot)
      dev.off()
    }
    return()
  }


  #     a and b, intersect(a,b) gives uniques
  #     a not b  setdiff(a,b) gives uniques
  #     b not a  setdiff(b,a) gives uniques
  #     a or b,  union(a,b) gives uniques

  a.in.b <- a %in% b
  b.in.a <- b %in% a

  total.in.a.only <- sum(!a.in.b) # not just unique a
  total.in.b.only <- sum(!b.in.a) # not just unique b

  unique.in.a.only <- length(setdiff(a,b)) # should be same as length( setdiff(a,b) )
  unique.in.b.only <- length(setdiff(b,a)) # should be same as length( setdiff(b,a) )

  if (!values | venn_draw | venn_save) {
    results <- data.frame( rbind(
      unique = c(
        in.a =  length(unique(a)),
        in.a.only = unique.in.a.only,
        overlap = length(intersect(a,b)),
        in.b.only = unique.in.b.only,
        in.b =  length(unique(b)),
        in.one.only = unique.in.a.only + unique.in.b.only,
        union = length(union(a,b))
      ),
      total = c(
        in.a =  length(a),
        in.a.only =   total.in.a.only,
        overlap = length(intersect(a,b)), # wrong: sum(a.in.b) + sum(b.in.a),
        in.b.only =   total.in.b.only,
        in.b =  length(b),
        in.one.only = total.in.a.only + total.in.b.only,
        union = length(a) + length(b)
      )
    ), stringsAsFactors = FALSE)
  }

  if (venn_draw) {
    grid::grid.newpage()
    VennDiagram::draw.pairwise.venn(
      area1 = results['unique', ]$in.a,
      area2 = results['unique', ]$in.b,
      cross.area = results['unique', ]$overlap,
      category = ab_names,
      fill = ab_colors,
      ...
    )
  }
  if (venn_save) {
    png(filename = filename )
    VennDiagram::draw.pairwise.venn(
      area1 = results['unique', ]$in.a,
      area2 = results['unique', ]$in.b,
      cross.area = results['unique', ]$overlap,
      category = ab_names,
      fill = ab_colors,
      ...
    )
    dev.off()
  }
  if (values) {

    # PROVIDE A LOGICAL data.frame OF ALL unique NAMES & WHERE EACH IS FOUND:
    x <- unique(c(a, b[!b.in.a])) # like unique(union(a,b)) but in a particular order, showing all unique a first.
    xina <- x %in% a
    xinb <- x %in% b
    results <- data.frame(
      value = x,
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

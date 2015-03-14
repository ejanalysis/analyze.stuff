#' @title Change some or all of the colnames of a data.frame or matrix via a 1-1 map
#'
#' @description
#' Returns a new set of field names, based on the old set of names, which can be specified in a file or as parameters.
#' This provides a convenient way to specify which names will be replaced with which new names, via a map of 1-1 relationships between the old names and new names.
#'
#' @details
#' This function returns a character vector of length equal to the number of oldnames (the parameter or the field in the file).
#' #' \cr\cr
#' @param allnames Character vector, optional. A vector of all the original fieldnames, such as the results of names(mydataframe).
#' @param oldnames Character vector, optional. A vector of only those original fieldnames that you want to change, in any order.
#' @param newnames Character vector, optional. A vector of new names, sorted in an order corresponding to oldnames.
#' @param file Character, optional. A filename (or path with filename) for a mapping file that is a csv file with two columns named with a header row: oldnames, newnames (instead of passing them to the function as parameters).
#' @param sort Logical value, optional, FALSE by default. If FALSE, return new fieldnames. If sort=TRUE, return vector of indexes giving new position of given field, based on sort order of oldnames.
#' @return A vector of character strings, the full set of fieldnames,
#' with some or all updated if sort=FALSE (default). Uses oldnames and newnames, or file for mapping. If those are not specified, it tries to open an interactive window for editing a mapping table to create and save it as a csv file.\cr\cr
#' If sort=TRUE, return vector of indexes giving new position of given field, based on sort order of oldnames.\cr\cr
#' If sort=TRUE, names in oldnames that are not in allnames are ignored with warning, & names in allnames that are left out of oldnames left out of new sort order indexes.
#' @seealso \code{\link{put.first}} which make it easier to rearrange the order of columns in a data.frame.
#' @examples
#' oldnames <- c('PCTILE', 'REGION')
#' newnames <- c('percentile', 'usregion')
#' df <- data.frame(REGION=301:310, ID=1:10, PCTILE=101:110, OTHER=1:10)
#' names(df) <- change.fieldnames(names(df), oldnames, newnames); names(df)
#' names(df) <- change.fieldnames(names(df), "ID", "identification"); names(df)
#' # names(df) <- change.fieldnames(names(df)); names(df)  # does not work on MacOSX?
#' # names(df) <- change.fieldnames(names(df), 'saved fieldnames.csv'); names(df)
#' df[ change.fieldnames(names(df), c('ID', 'OTHER', 'REGION', 'PCTILE'), sort=TRUE)]
#' # much like df[ , c('ID', 'OTHER', 'REGION', 'PCTILE') ]
#' #  change.fieldnames is more useful when file specified
#' @export
change.fieldnames <- function(allnames, oldnames, newnames, file=NA, sort=FALSE) {

	if (!is.na(file)) {
	  if (!missing(oldnames) | !missing(newnames)) {stop('Cannot specify file and also oldnames or newnames')}
	  changes <- read.csv(file, as.is=TRUE)
	  if (!( ('oldnames' %in% names(changes) ) & ('newnames' %in% names(changes)) )) {
      if (sort==FALSE) { stop('file must have columns named oldnames and newnames') }
	  }

	  if (sort==FALSE) {
	    oldnames <- changes$oldnames
      newnames <- changes$newnames
	  } else {
      # if just using file for sort of colnames, assume the first column of csv is the new sort order even if lacks colname
      warning('assuming first column in file contains fieldnames in the new sort order')
      oldnames <- changes[ , 1]
	  }
	  if (missing(allnames)) {allnames <- oldnames}
	  # ASSUME THAT IF ONLY file IS SPECIFIED, THE FIRST COLUMN HAS ALL THE NAMES, NOT JUST ONES TO CHANGE, BUT THEN FUNCTION SIMPLY RETURNS newnames FROM file
	}

	if (is.na(file) & missing(allnames)) {stop('Must specify allnames if file not specified')}  # specifying only file='blah.csv'  works
	if (!is.vector(allnames) | any(!is.character(allnames))) {stop('allnames must be a vector of character type fieldnames')}

	if (!sort) {
    if (is.na(file) & sum(missing(oldnames), missing(newnames))==1 ) {
      stop('Must specify oldnames, newnames in 2-column csv file or as parameters, or specify none of the 3 for interactive editing of names (unless sort=TRUE)')
    }
	}

	if (is.na(file) & missing(oldnames) & missing(newnames)) {
	  changes <- edit(data.frame(oldnames=allnames, newnames=allnames, stringsAsFactors=FALSE))
	  write.csv(changes, file='saved fieldnames.csv', row.names=FALSE)
	  cat('\n old and new names saved in csv file called:   "saved fieldnames.csv"\n')
	  oldnames <- changes[ , 1]
	  newnames <- changes[ , 2]
	}

	if (!is.vector(oldnames) | any(!is.character(oldnames))) {stop('oldnames must be a vector of character type fieldnames\n')}
	if (!sort) { if (!is.vector(newnames) | any(!is.character(newnames))) {stop('newnames must be a vector of character type fieldnames\n')} }
	if (!sort) { if (length(oldnames)!=length(newnames)) {stop('oldnames and newnames must be the same length\n')} }
	if (!sort) { if (length(allnames)==0 | length(oldnames)==0 | length(newnames)==0  ) {stop('no input can be length zero\n')} }

	if (length(allnames) < length(oldnames)) {cat('Warning: length(allnames) generally should be >= length(oldnames)\n')}

  # Done with error-checking or file-creation/editing.
	################

	# Just replace the ones that match up, so
	#   if allnames has something not in the oldnames, newnames entries, that is just left unchanged in allnames.
	#   if oldnames has something that is not in allnames, that is ignored.

  if (!sort) {
    # return vector of new fieldnames in same order as they were passed to this function
    newnames <- newnames[oldnames %in% allnames]
    oldnames <- oldnames[oldnames %in% allnames]
    allnames[match(oldnames, allnames)] <- newnames
    return(allnames)
  } else {
    # return vector of new positions for the columns whose names were passed to this function, using sort order found in oldnames
    oldnames <- oldnames[oldnames %in% allnames]
    newposition <- match(oldnames, allnames)
    return(newposition)
  }
}

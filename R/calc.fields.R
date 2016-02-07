#' @title Create calculated fields by specifying formulas
#'
#' @description
#' Create calculated fields from formulas that are specified as character strings, returning data.frame of specified results (not all intermediate variables necessarily)
#' e.g., create calculated demographic variables from raw American Community Survey counts.
#' This function is useful if you are working with a dataset with numerous fields,
#' and you want to calculate numerous derived fields from those original fields,
#' and you find it convenient to store all of the formulas in a text file, for example.
#' You could read in the formulas from the file, and apply them to a new version of the dataset
#' to calculate a new version of all of your derived fields.
#'
#' @details
#' This function returns a matrix or vector of results of applying specified formulas to the fields in the input data.frame.
#' Each row of data is used in a formula to provide a row of results.
#' @param mydf Required. A data.frame with strings that are field names (input variables) that may appear in formulas. See example.
#' @param formulas Required. A vector of strings that are formulas based on input variables and/or variables calculated from previous formulas. See example.
#' @param keep Optional. A vector of strings that are the input and/or calculated variables to return, in case not all intermediate variables are needed.
#'   Default is all results of formulas but not any input variables.
#' @return A data.frame of new variables where columns are defined by keep (or all calculated variables if keep is not specified).
#' @seealso \code{\link{change.fieldnames}}
#' @examples
#' # formulas could be ejscreenformulas$formula from the ejscreen package, for example.
#'  myforms <- c('calcvar1 = b+1', 'calcvar2=calcvar1 + a', 'calcvar3<- paste(a,"x",sep="")')
#'  # Saving to and reading from a file that stores all these formulas:
#'  # write.csv(myforms, file='testforms.csv', row.names = FALSE)
#'  # myforms <- read.csv('testforms.csv')
#'  mydat  <- data.frame(a=1:10, b=2:11)
#'  x <- calc.fields(mydat, myforms); x
#'  # Return only some of the input/output variables:
#'  calc.fields(mydf=mydat, formulas=myforms, keep=c('a', 'calcvar2','calcvar3') )
#' @export
calc.fields <- function(mydf, formulas, keep) {
  if (missing(keep)) {
    # This will only work if the calculated variable is followed by a space, or = sign, or <- in the formula
    keep <- substr(formulas, 1, regexpr('[<= ]', formulas) -1)
  }

  formulas <- formulas[!is.na(formulas)]

  #  cat('\n formulas: ', formulas,'\n\n')
  #  cat('\n keep: ', keep,'\n\n')

  # make fields in mydf available for use in formulas and exists()
  # Just a quick workaround would be to ensure mydf$cancer exists, since we know it conflicts with cancer data in survival package.
  # This at least avoids a crash if a formula mentions cancer (only a problem if expected as full colname in mydf) but mydf lacks cancer as a field, and survival package is loaded.
  # it will create a useless, perhaps misleading result, but won't crash by trying to use cancer data from that package in place of mydf$cancer.
  if (!('cancer' %in% names(mydf)) & any(grepl('cancer', formulas))) {mydf$cancer=0}

  suppressMessages(attach(mydf))
  # on.exit(detach(mydf))
  tryCatch({
    for (thisformula in formulas) {

      # Trying to handle cases where formula relies on some variable that was not provided in mydf

      # A problem arises if a formula relies on a variable that is not in mydf but is used by some loaded package,
      # for example if mydf does not contain the field called cancer but the survival package (required by Hmisc which is required by the ejscreen pkg) provides a dataset lazyloaded called cancer,
      # so the formula appears to work, but it operates on the cancer from the survival package instead of the intended but missing mydf$cancer
      # So we would ideally check the formula's inputs to see that they are all in mydf, rather than just trying to see if the formula works after attaching mydf.
      #    textform <- strsplit(gsub('.*<-', '', parse(text=thisformula) ), split=' |[[:punct:]]| ' )[[1]]
      #    textform <- textform[textform!='']
      # that gets the words from the right side of the formula, but only if it is written as ....<-....
      # and it also gets functions like sum, min, max, mean, etc. so it can't really be used to check if each variable is in mydf since some are functions not variables in the formula.
      # and it splits up on _ etc.
      # Just a quick workaround used above was to ensure mydf$cancer exists, since we know it conflicts with cancer data in survival package.

      y=try( eval(parse(text=thisformula) ), silent = TRUE)
      if (class(y)=="try-error") {
        cat('Cannot use formula: '); print(as.character(parse(text=thisformula)))
      } else {
        eval(parse(text=thisformula) )
      }
    }

    # RETURN ONLY THE ONES SUCCESSFULLY CREATED, OUT OF ALL REQUESTED TO BE KEPT
    # attach() allows exists function to see fields in mydf, as specified by keep
    # but it does not allow mget() to obtain them!

    #print(' keep at first: '); print(keep)
    #print('in memory now: ls() = '); print(ls())
    #print(' keep now after using exists(): '); print(keep)
    # note this will crash with error if keep is invalid, such as '' or NA or c('', '') etc.
    keep.from.mydf <- keep[keep %in% names(mydf)]
    keep.other <- keep[!(keep %in% keep.from.mydf)]
    #print(' keep.from.mydf: '); print(keep.from.mydf)
    #print(' keep.other: '); print(keep.other)
    #print('ls() ');print(ls())
    #print('sapply exists ' ); print(sapply(keep.other, FUN=exists))
    # DOES NOT WORK SO USE ls() instead:  #if (length(keep.other) > 0) {keep.other <- keep.other[sapply(keep.other, FUN=exists)] }
    if (length(keep.other) > 0) {keep.other <- keep.other[keep.other %in% ls()] }
    # print(' keep.other after check if exists: '); print(keep.other)

    # COULD ADD WARNINGS HERE ABOUT VARIABLES USER ASKED TO KEEP THAT DO NOT EXIST

    if (length(keep.other) > 0) {
      outdf <- data.frame( mydf[ , keep.from.mydf, drop=FALSE], mget(keep.other), stringsAsFactors=FALSE)
    } else {
      outdf <- data.frame( mydf[ , keep.from.mydf, drop=FALSE], stringsAsFactors=FALSE)
    }

    suppressMessages(detach(mydf))

    return(outdf)
    # #

  }, finally = if ('mydf' %in% search()) {detach(mydf)} )

}

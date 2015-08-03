#' @title Create calculated fields by specifying formulas
#'
#' @description
#' Create calculated fields from formulas that are specified as character strings, returning data.frame of specified results (not all intermediate variables necessarily)
#' e.g., create calculated demographic variables from raw American Community Survey counts.
#'
#' @details
#' This function returns a matrix or vector of results of applying specified formulas to the fields in the input data.frame.
#' Each row of data is used in a formula to provide a row of results.
#' @param mydf Required. A data.frame with strings that are field names that may appear in formulas. e.g., mydf=data.frame(a=1:10, b=2:11)
#' @param formulas Required. A vector of strings that are formulas such as "formulas=c('calcvar1 = b+1', 'calcvar2=calcvar1 + a')"
#' @param keep Optional. A vector of strings that are calculated variables to return, in case not all intermediate variables are needed. Default is all results of formulas.
#' @return A data.frame of new variables where columns are defined by keep (or all calculated variables if keep is not specified).
#' @seealso \code{\link{change.fieldnames}}
#' @examples
#' calc.fields(mydf=data.frame(a=1:10, b=2:11),
#'   formulas=c('calcvar1 = b+1', 'calcvar2=calcvar1 + a', 'calcvar3<- paste(a,"x",sep="")'),
#'   keep=c('calcvar2','calcvar3'))
#' @export
calc.fields <- function(mydf, formulas, keep) {
  if (missing(keep)) {
    # This will only work if the calculated variable is followed by a space, or = sign, or <- in the formula
    keep <- substr(formulas, 1, regexpr('[<= ]', formulas) -1)
  }

  warning('not working yet') # ***********************

  suppressMessages(attach(mydf))

  for (thisformula in formulas) {

    # Trying to handle cases where formula relies on some variable that was not provided in mydf

    y=try( eval(parse(text=thisformula) ), silent = TRUE)
    if (class(y)=="try-error") {
      cat('Cannot use formula: '); print(thisformula)
    } else {
      eval(parse(text=thisformula) )
    }

    #     y=tryCatch( eval(parse(text=thisformula) ),
    #                 error=function(x) {print('problem with formula'); invokeRestart()})

  }
  suppressMessages(detach(mydf))

  # RETURN ONLY THE ONES SUCCESSFULLY CREATED, OUT OF ALL REQUESTED TO BE KEPT
  keep <- keep[sapply(keep, FUN=exists)]

  outdf <- data.frame( mget(keep), stringsAsFactors=FALSE)
  return(outdf)
  # #
}

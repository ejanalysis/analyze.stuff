% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc.fields.R
\name{calc.fields}
\alias{calc.fields}
\title{Create calculated fields by specifying formulas as text, strings}
\usage{
calc.fields(mydf, formulas, keep)
}
\arguments{
\item{mydf}{Required. A data.frame with strings that are field names (input variables) that may appear in formulas. See example.}

\item{formulas}{Required. A vector of strings that are formulas based on input variables and/or variables calculated from previous formulas. See example.}

\item{keep}{Optional. A vector of strings that are the input and/or calculated variables to return, in case not all intermediate variables are needed.
Default is all results of formulas but not any input variables.}
}
\value{
A data.frame of new variables where columns are defined by keep (or all calculated variables if keep is not specified).
}
\description{
Create calculated fields from formulas that are specified as character strings,
returning data.frame of specified results (not all intermediate variables necessarily)
}
\details{
May get replaced with functions from EJAM package. See source code here for notes.

This function returns a matrix or vector of results of applying specified formulas to the fields in the input data.frame.
Each row of data is used in a formula to provide a row of results.

e.g., create calculated demographic variables from raw American Community Survey counts.
This function is useful if you are working with a dataset with numerous fields,
and you want to calculate numerous derived fields from those original fields,
and you find it convenient to store all of the formulas in a text file, for example.
You could read in the formulas from the file, and apply them to a new version of the dataset
to calculate a new version of all of your derived fields.

WARNING: This function did what I needed but probably
fails if mydf has any column names that are also variables
in the calling or global environment?
May need to more carefully specify environment in the eval() and or ls() steps.
}
\examples{
 myforms <- c('bsquared = b^2', 'that.plus.a=bsquared + a',
   'result <-min(that.plus.a,b)')
 mydat  <- data.frame(a=1:-2, b=2:5)
 x <- calc.fields(mydat, myforms)
 cbind(mydat, x)
 # Return only some of the input/output variables:
 calc.fields(mydf=mydat, formulas=myforms, keep=c('b', 'that.plus') )

myforms <- c('bplus1 = b+1', 'that.plus.a=bplus1 + a',
  'xfold <- ifelse(is.na(a), "(no a)", paste(a,"\% !",sep=""))',
   'there.is.no.y = y/0')
mydat  <- data.frame(a=c(104:106,NA), b=c(1:3,0))
x <- calc.fields(mydat, myforms)
data.frame(formula = rbind(paste0('  ', myforms, '  ')))
cbind(mydat, x)

# formulas could be ejscreen::ejscreenformulas$formula, for example.
 # Saving to and reading from a file that stores all these formulas:
 # write.csv(myforms, file='testforms.csv', row.names = FALSE)
 # myforms <- read.csv('testforms.csv')

}
\seealso{
\code{\link[=change.fieldnames]{change.fieldnames()}}
}

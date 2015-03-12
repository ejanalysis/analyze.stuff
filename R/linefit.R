#' @title Add fit lines to a scatter plot
#'
#' @description
#' Convenient wrapper for lowess(), lm(), and coef(line())
#'
#' @details
#' This function adds lines to a scatter plot, using lines(\code{\link{lowess}}(x,y)), abline(\code{\link{lm}}(y~x)), and abline(\code{\link{coef}}(\code{\link{line}}(x,y)))
#' DOESN'T SEEM TO WORK IF log='xy' was used in original plot()
#' NOTE: coef(line())  and lm()  give different results
#' @param x x values, required
#' @param y y values, required
#' @param type passed through to lines() for the lowess
#' @param cex scaling for lowess
#' @param show.lowess Logical value, optional, TRUE by default. Defines if lowess is shown
#' @param show.line Logical value, optional, TRUE by default. Defines if should show abline(coef(line(x,y)))
#' @return Provides a plot just as a side effect
#' @examples
#'     # see
#' #?lm  or  ?aov   or  ?glm
#' # ?line
#' require(graphics)
#' plot(cars)
#' (z <- line(cars))
#' abline(coef(z))
#' ## Tukey-Anscombe Plot :
#' plot(residuals(z) ~ fitted(z), main = deparse(z$call))
#' # ?predict
#' # ?lowess
#' # ?scatterplot
#' # The scatterplot( ) function in the car package offers many enhanced features, including
#' #    fit lines, marginal box plots, conditioning on a factor, and interactive point identification. Each of these features is optional.
#' # Enhanced Scatterplot of MPG vs. Weight
#' # by Number of Car Cylinders
#'  library(car)
#'  scatterplot(mpg ~ wt | cyl, data=mtcars,
#'              xlab="Weight of Car", ylab="Miles Per Gallon",
#'              main="Enhanced Scatter Plot",
#'              labels=row.names(mtcars))
#' @export
linefit <- function(x, y, type='b', cex=4, show.lowess=TRUE, show.lm=TRUE, show.line=TRUE) {
  if (show.lowess) { lines(lowess(x, y), type=type, col="blue", pch='.', cex=cex) } # lowess line (x, y)
  if (show.lm)     { abline(lm(y ~ x, na.action=na.exclude), col="dark green") } # regression line (y ~ x)
  if (show.line)   { abline(coef(line(x,y)), col='light green') }
}

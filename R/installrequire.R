#' @title Require a list of packages, installing if necessary
#'
#' @description
#' Convenient way to specify packages to attach, and install any that are not already installed.
#'  It only installs a package if that package is not already available locally.
#'
#' @details
#'  Uses \code{\link{require}} and if necessary uses \code{\link{install.packages}} or \code{\link{devtools::install_github}}
#'  If no parameters, prints an example. If github = 'ej' it installs several from github.
#' @param x vector of package names e.g., c("Hmisc", "data.table")
#' @param github optional vector of user slash package names e.g., "rstudio/shiny"
#' @return none
#' @examples installrequire()
#' @export
installrequire <- function(x, github) {

  ##################################
  # load (first downloading and installing if necesssary), each of several packages we need, including any from github
  ##################################

  if (missing(x)) {
    pkgs       <- NULL # could set defaults here
    if (missing(github)) {
      cat('Example of usage:\n')
      cat('pkgsgithub <- c(\n',
        '"rstudio/shiny",\n',
        '"ejanalysis/analyze.stuff",\n',
        '"ejanalysis/ejanalysis",\n',
        '"ejanalysis/proxistat",\n',
        '"ejanalysis/ejscreen",\n',
        '"ejanalysis/ACSdownload",\n',
        '"ejanalysis/countyhealthrankings",\n',
        '"ejanalysis/UScensus2010blocks"\n',
      ')\n',
      'installrequire("data.table", pkgsgithub)\n')
      # regarding 'ejanalysis/analyze.stuff', and other such packages, see http://www.ejanalysis.com
      #   if (!require('devtools')) install.packages('devtools')
      #   devtools::install_github("ejanalysis/analyze.stuff")
      #   devtools::install_github("ejanalysis/countyhealthrankings")
      #   devtools::install_github("ejanalysis/UScensus2010blocks")
      #   devtools::install_github("ejanalysis/ACSdownload")
      #   devtools::install_github(c("ejanalysis/proxistat")
      #   devtools::install_github(c("ejanalysis/ejanalysis")
      #   devtools::install_github("ejanalysis/ejscreen")

    }
  } else {
    pkgs <- x
  }

  if (missing(github)) {
    pkgsgithub <- NULL
  } else {
    pkgsgithub <- github
    if (github == 'ej') {
      pkgsgithub <- c(
        'rstudio/shiny',
        'ejanalysis/analyze.stuff',
        'ejanalysis/ejanalysis',
        'ejanalysis/proxistat',
        'ejanalysis/ejscreen',
        'ejanalysis/ACSdownload',
        'ejanalysis/countyhealthrankings',
        'ejanalysis/UScensus2010blocks'
      )
      # regarding 'ejanalysis/analyze.stuff', and other such packages, see http://www.ejanalysis.com
      #   if (!require('devtools')) install.packages('devtools')
      #   devtools::install_github("ejanalysis/analyze.stuff")
      #   devtools::install_github("ejanalysis/countyhealthrankings")
      #   devtools::install_github("ejanalysis/UScensus2010blocks")
      #   devtools::install_github("ejanalysis/ACSdownload")
      #   devtools::install_github(c("ejanalysis/proxistat")
      #   devtools::install_github(c("ejanalysis/ejanalysis")
      #   devtools::install_github("ejanalysis/ejscreen")

    }

  }

  for (mypkg in pkgs) {
    if (!library(mypkg, character.only = TRUE, logical.return = TRUE)) {install.packages(mypkg); library(mypkg, character.only=TRUE)}
  }

  for (mypkg in pkgsgithub) {
    if (!library( gsub('^.*/', '', mypkg), character.only = TRUE, logical.return = TRUE)) {
      if (!library('devtools', logical.return = TRUE)) {
        install.packages('devtools')
        library('devtools') # get at least the cran version of devtools to help get pkgs from github
      }
      devtools::install_github(mypkg)
    }
  }

}



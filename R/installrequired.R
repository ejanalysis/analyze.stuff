#' @title Require a list of packages, downloading and installing if necessary
#' @description
#' Convenient way to specify packages to attach, and install any that are not already installed.
#'  It only installs a package if that package is not already available locally.
#' @details
#'  Uses \code{\link{require}} and if necessary uses \code{\link{install.packages}} or \code{\link[devtools]{install_github}}
#'  If no parameters, prints an example.
#' @param x vector of package names e.g., c("Hmisc", "data.table")
#' @param github optional vector of user slash package names e.g., "rstudio/shiny" but those can just be in x now.
#'  If github = 'ej' it installs several specific ones from github (also see \url{http://www.ejanalysis.com}):
#'  \itemize{
#'   \item 'rstudio/shiny'
#'   \item 'ejanalysis/analyze.stuff'
#'   \item 'ejanalysis/ejanalysis'
#'   \item 'ejanalysis/proxistat'
#'   \item 'ejanalysis/ejscreen'
#'   \item 'ejanalysis/ACSdownload'
#'   \item 'ejanalysis/countyhealthrankings'
#'   \item 'ejanalysis/UScensus2010blocks'
#'  }
#' @param gitlatest Optional logical, default is FALSE which means not downloaded from github if pkg of that name is already installed. If TRUE, download latest from github even if already installed.
#' @return  NULL
#' @examples
#'   \dontrun{
#'   installrequired('stringr')
#'   installrequired('rstudio/shiny')
#'   #
#'   installrequired(c('Hmisc' , 'ejanalysis/analyze.stuff'))
#'   # or
#'   installrequired('ej') # for several specific ones used in e
#'   }
#' @export
installrequired <- function(x, github, gitlatest = FALSE) {

  ################################# #
  # load (first downloading and installing if necesssary), each of several packages we need, including any from github
  ################################# #

  if (missing(x)) {
    pkgs       <- NULL # could set defaults here
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
    }
  }

  # Try to identify any github-based pkgs that were passed in x, and handle appropriately
  looksLikeRepo <- grepl(pattern = '^.*/', pkgs)
  pkgsgithub    <- c(pkgsgithub, pkgs[looksLikeRepo])
  pkgs <- pkgs[!looksLikeRepo]

  # load each pkg, first installing if necessary

  for (mypkg in pkgs) {
    if (!library(mypkg, character.only = TRUE, logical.return = TRUE)) {install.packages(mypkg); library(mypkg, character.only = TRUE)}
  }

  for (mypkg in pkgsgithub) {
    if (!library( gsub('^.*/', '', mypkg), character.only = TRUE, logical.return = TRUE) | gitlatest) {
      # install devtools if do not already have it
      if (!library('devtools', logical.return = TRUE)) {
        install.packages('devtools')
        #library('devtools') # get at least the cran version of devtools to help get pkgs from github
      }
      devtools::install_github(mypkg)
    }
  }
  return(NULL)
}

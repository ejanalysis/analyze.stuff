#' @export
required.packages <- function(pkgs) {
  
  # does library() for each of the pkgs specified in character vector pkgs
  # but downloads and installs the ones that fail require() because they are not yet installed
  
  # and handles packages in github repos as well
  # e.g., 
  # pkgs <- c('lubridate', 'plotrix', 'readxl') 
  # pkgs <- c('Hmisc' , 'ejanalysis/analyze.stuff')
  
  looksLikeRepo <- grepl(pattern = '^.*/', pkgs)
  gitpkgs    <- pkgs[ looksLikeRepo]
  normalpkgs <- pkgs[!looksLikeRepo]
  
  for (mypkg in normalpkgs) {
    if (!library(mypkg, character.only = TRUE, logical.return = TRUE)) {install.packages(mypkg); library(mypkg, character.only=TRUE)}  
  }
  
  for (mypkg in gitpkgs) {
    if (!library( gsub('^.*/', '', mypkg), character.only = TRUE, logical.return = TRUE)) {
      # install devtools if do not already have it
      if (!library('devtools', logical.return = TRUE)) {
        install.packages('devtools')
        library('devtools') # get at least the cran version of devtools to help get pkgs from github
      }
      devtools::install_github(mypkg)
    }
  }
}

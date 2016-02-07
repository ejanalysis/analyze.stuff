#' @title Counts lines of source code in .R files of package source
#' @description
#'  This is just a way to summarize how many lines of code appear to be in the .R files in the folder that a package is built from.
#' @param folder Default is current working directory. **This is NOT the base path of the package itself!
#'  It is the full path of the folder within which is a folder for each package of interest.
#'  For example, folder= '~/Documents/R PACKAGES' works but folder= '~/Documents/R PACKAGES/mypkg' does not see the package called mypkg
#' @param packages Default is all found in folder. Can specify a subset of those by name as character vector.
#' @param recursive Default is TRUE, searches subfolders within specified folder.
#' @param sums Default is FALSE, but if TRUE it returns the count of .R files and lines of code for each package found.
#' @return Returns a data.frame of results, with details depending on sums parameter.
#'  Also prints summary info if sums=FALSE, and returns detailed info.
#'
#' @examples
#'
#'  \dontrun{
#'  linesofcode(folder= '~/Documents/R PACKAGES', packages=c('analyze.stuff', 'proxistat') )
#'  x <- linesofcode(folder= '~/Documents/R PACKAGES')
#'  x[order(x$code), c('filename', 'package', 'code')]
#'  }
#' @export
linesofcode <- function(folder=getwd(), packages, recursive=TRUE, sums=FALSE) {

  # count lines of code vs comments in a folder of .R files
  # or each of the "packages" directories directly under "folder"
  # e.g., linesofcode(folder= '~/Documents/R PACKAGES', packages=c('analyze.stuff', 'proxistat') )

  # if we were looking at installed rather than source code:
  #   packages.all.name <- basename(packages.all.fullpath <- find.package()) # faster than installed.packages()
  #   packages.all.dir <- dirname(packages.all.fullpath)
  #   if (missing(packages)) {packages <- packages.all.name}
  #   if (any(!(packages %in% packages.all.name))) {stop('packages specified cannot be found among installed packages')}
  #
  #   if (missing(folder)) {
  #     folder <- packages.all.dir #installed.packages()[packages, 'LibPath', drop=FALSE]  # is slow
  #   }
  #
  # Rfilenames <- list.files(path=folder, pattern = '.R') # also finds .RData which we do not want

  for (myfolder in folder) {
    #Rfilenames <- dir2('*.R', path=myfolder, recursive=recursive)
    #print ( dir2('*.R', path=myfolder, recursive=recursive) )
  }
  Rfilenames <- dir2('*.R', path=folder, recursive=recursive)

  justfilename.allfound     <- gsub('[[:print:]]*/', '', Rfilenames)
  pkgname.allfound          <- gsub('/[[:print:]]*', '', Rfilenames)

  if (!missing(packages)) {
    # remove files not within packages trees
    if (!any(packages %in% pkgname.allfound)) {stop('None of specified packages were found under specified folder')}
    if (!all(packages %in% pkgname.allfound)) {warning('Not all specified packages were found under specified folder')}
    packages <- packages[packages %in% pkgname.allfound]
    #Rfilenames <- Rfilenames[ grep('^[[:alphanum:]].///' , Rfilenames) ]
  } else {
    packages <- pkgname.allfound
  }

  Rfilenames   <- Rfilenames[pkgname.allfound %in% packages]
  justfilename <- justfilename.allfound[ pkgname.allfound %in% packages ]
  pkgname      <- pkgname.allfound[pkgname.allfound %in% packages ]

  n <- length(Rfilenames)
  if (n > 0) {

    out <- matrix(nrow=n, ncol = 6)
    out <- as.data.frame(out)
    names(out) <- c('lines', 'code', 'comments', 'package', 'filename', 'path')

    for (i in 1:n) {

      filetext <- readLines(file.path(folder, Rfilenames[i]))
      linecount <- length(filetext)
      commentcount <-  sum( grepl(pattern = '^#', x =  filetext, ignore.case = TRUE) )
      codecount <- linecount - commentcount

      out[i, 'lines'] <- linecount
      out[i, 'comments'] <- commentcount
      out[i, 'code'] <- codecount
      out[i, 'path'] <- Rfilenames[i]
      out[i, 'package'] <- pkgname[i]
      out[i, 'filename'] <- justfilename[i]
      #out[i, ''] <- x
      #cat('Lines: ', linecount, ' in', Rfilenames[i], '(', codecount, 'code +', commentcount, 'comments)', '\n')
    }

    mysums <- cbind(
      Hmisc::summarize(out$lines, by=out$package, FUN=sum),
      Hmisc::summarize(out$filename, by=out$package, FUN=length)
    )
    mysums <- mysums[ , c(1,4,2)]
    names(mysums) <- c("package", "filename", "lines")

    if (sums) {
      return(mysums)
    } else {
      cat('\n'); print( mysums ); cat('\n')
      return(out)
    }

  } else {
    cat('No .R files found in', folder, '\n')
  }

}

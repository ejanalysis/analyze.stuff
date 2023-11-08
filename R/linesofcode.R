#' @title Counts lines of source code in .R files of package source - NOT TESTED
#' @description
#'  This is just a way to summarize how many lines of code appear to be in the .R files in the folder that a package is built from.
#' @param folder Default is current working directory. **This is NOT the base path of the package itself!
#'  It is the full path of the folder within which is a folder for each package of interest.
#'  For example, folder= '~/Documents/R PACKAGES' works but folder= '~/Documents/R PACKAGES/mypkg' does not see the package called mypkg
#' @param packages Default is all found in folder. Can specify a subset of those by name as character vector.
#' @param recursive Default is TRUE, searches subfolders within specified folder.
#' @param sums Default is FALSE, but if TRUE it returns the count of .R files and lines of code for each package found.
#' @param cropfilename number of characters displayed in console
#' @param croppath     number of characters displayed in console
#' @param showrows number of rows displayed in console
#' @param rfolderonly optional
#' @return Returns a data.frame of results, with details depending on sums parameter.
#'  Also prints summary info if sums=FALSE, and returns detailed info.
#' @examples
#'
#'  \dontrun{
#'  linesofcode(folder= '..', packages=c('analyze.stuff', 'proxistat') )
#'  x <- linesofcode(folder= '~/Documents/R PACKAGES')
#'  x[order(x$code), c('filename', 'package', 'code')]
#'  }
#' @export
linesofcode <- function(folder=getwd(), packages, recursive=TRUE, sums=FALSE, rfolderonly=FALSE, cropfilename=40, croppath=20, showrows=NULL) {

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
    names(out) <- c('lines', 'comments', 'code', 'package', 'where', 'filename')

    for (i in 1:n) {

      filetext <- suppressWarnings(  readLines(file.path(folder, Rfilenames[i])) )
      linecount <- length(filetext)
      commentcount <-  sum( grepl(pattern = '^#', x =  filetext, ignore.case = TRUE) )
      codecount <- linecount - commentcount

      out[i, 'lines'] <- linecount
      out[i, 'comments'] <- commentcount
      out[i, 'code'] <- codecount
      out[i, 'package'] <- pkgname[i]
      out[i, 'where'] <- gsub(justfilename[i], "", gsub(pkgname[i], "", Rfilenames[i]))
      out[i, 'filename'] <- justfilename[i]
      #out[i, ''] <- x
      #cat('Lines: ', linecount, ' in', Rfilenames[i], '(', codecount, 'code +', commentcount, 'comments)', '\n')
    }
    out <- out[order(out$lines, decreasing = T), ]
    rownames(out) <- NULL

    if (rfolderonly) {out <- out[out$where == "/R/", ]}

    mysums <- cbind(
      Hmisc::summarize(out$lines,    by=out$package, FUN=sum),
      Hmisc::summarize(out$filename, by=out$package, FUN=length)
    )
    mysums <- mysums[ , c("out$package", "out$filename", "out$lines")]
    names(mysums) <- c("package", "filename", "lines")
    mysums <- mysums[order(mysums$lines, decreasing = T), ]
    rownames(mysums) <- NULL
    if (sums) {
      return(mysums)
    } else {
      cat('\n'); print( mysums ); cat('\n')
      cropped <- out
      cropit <- function(x,n) {x[nchar(x) > n+3] <- paste0(substr(x[nchar(x) > n+3],1,n), "..." ); return(x)}
      cropped$filename  <- cropit(cropped$filename, cropfilename)
      cropped$where    <- cropit(cropped$where,     croppath)
      if (is.null(showrows)) {
        showrows <- 1 + findInterval(sum(cropped$lines)/2, cumsum(cropped$lines))
        cat("\nMost of the code is in these files: \n\n")}
      print(cropped[1:(min(NROW(cropped), showrows )),])
      cat("\n Full list is returned invisibly \n")
      invisible(out)
    }

  } else {
    cat('No .R files found in', folder, '\n')
  }

}

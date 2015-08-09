#' @export
linesofcode <- function(folder=getwd(), packages, recursive=TRUE, sums=FALSE) {

  # count lines of code vs comments in a folder of .R files
  # or each of the "packages" directories directly under base "folder"
  # e.g., linesofcode(folder= '~/Documents/R PACKAGES', packages=c('analyze.stuff', 'proxistat') )

  # Rfilenames <- list.files(path=folder, pattern = '.R') # also finds .RData which we do not want
  Rfilenames <- dir2('*.R', path=folder, recursive=recursive)

  pkgname <- gsub('/[[:print:]]*', '', Rfilenames)
  if (!missing(packages)) {
    # remove files not within packages trees
    Rfilenames <- Rfilenames[pkgname %in% packages]
    pkgname <- pkgname[pkgname %in% packages]
    #Rfilenames <- Rfilenames[ grep('^[[:alphanum:]].///' , Rfilenames) ]
  }
  justfilename <- gsub('[[:print:]]*/', '', Rfilenames)

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
    print( mysums )

    if (sums) {
      return(mysums)
    } else {
      return(out)
    }

  } else {
    cat('No .R files found in', folder, '\n')
  }

}

#' @title Unzip multiple zip files
#' @description Wrapper for [unzip()] which unzips a single file.
#' @param zipfile vector of names of files to unzip
#' @param files Optional, NULL by default which signifies all files in each zipfile will be extracted.
#' Otherwise, a list, with the nth element being a vector (length 1 or more) of character string names of files to extract from the nth zipfile.
#' @param exdir The directory to extract files to (the equivalent of unzip -d). It will be created if necessary.
#' @param unzip See help for unzip
#' @param overwrite Logical, optional, TRUE by default which means the local file is not overwritten if it already exists.
#' @param ... Other arguments passed through to unzip
#' @return Returns a list of the filepaths extracted to, from each zipfile. Names of list are the zip file names.
#' @export
unzip.files <- function(zipfile, files=NULL, exdir = ".", unzip='internal', overwrite=TRUE, ...) {
  if (!file.exists(exdir)) {
    dir.create(exdir)
  }
  result <- list()
  for (i in 1:length(zipfile)) {
    result[[i]] <- unzip(zipfile[i], files=files[[i]], exdir=exdir, overwrite=overwrite, unzip=unzip, ...)
  }
  names(result) <- zipfile
  return( result )
}

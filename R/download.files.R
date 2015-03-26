#' @title Try to download one or more files
#' @description Attempts to download files, given name(s) from specified url, saving them in specified folder.
#' Just a wrapper that Uses \code{\link{download.file}} which only downloads a single file.
#' @param url The url of folder with files to download, as character string
#' @param files A character vector of file names.
#' @param todir The folder where downloaded files will be placed, as a character string.
#' @param silent Logical, optional, FALSE by default. Prints a message using cat() if TRUE.
#' @param overwrite Optional, logical, FALSE by default. If FALSE, checks to see if file already exists in local folder and does not download if already exists.
#'   But note that may cause problems if zero size file exists already due to earlier failed download.
#' @return Returns vector of numbers, each being 1 or 0 or 2 to signify success or failure or no attempt because file already seems to exist locally.
#' @seealso \code{\link{download.file}}
#' @export
download.files <- function(url, files, todir, silent=FALSE, overwrite=FALSE) {

  downloadDone <- vector()
  # *** SOME NETWORKS HAVE FIREWALL RULES THAT DO NOT ALLOW R TO DOWNLOAD ZIP FILES VIA SOFTWARE LIKE THIS:

  for (i in 1:length(files)) {
    if (!(file.exists(file.path(todir, files[i])))) {
      if (!file.exists(file.path(url,files[i]))) {
        warning(paste('Cannot find local or remote copy of ',file.path(url,files[i])) )
        downloadDone[i] <- 0
      } else {
        downloadDone[i] <- download.file( file.path(url, files[i]), file.path(todir, files[i]))
        pause(3)
      }
    } else {
      if (overwrite) {
        # already have local copy but want to overwrite (but note did not check if exists remotely)
        downloadDone[i] <- download.file( file.path(url, files[i]), file.path(todir, files[i]))
        pause(3)
      } else {
        # already have local copy and do not want to overwrite
        downloadDone[i] <- 2
      }
    }
  }

  if (!silent) {
    cat('Checked: ', length(files), '\n')
    cat('  Already exist locally and do not want to overwrite: ', sum(downloadDone==2), '\n')
    cat('  Tried to download: ', sum(downloadDone!=2), '\n')
    cat('    Downloaded successfully: ', sum(downloadDone==1), '\n')
    cat('    Downloaded error: ', sum(downloadDone==0), '\n')
    cat('Errors: \n')
    print(cbind(files[downloadDone==0]))
  }
  return(downloadDone)
}


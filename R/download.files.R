#' @title Try to download one or more files
#' @description Attempts to download files, given name(s) all from one specified url, saving them in specified folder.
#'   Just a wrapper that Uses [download.file()] since that only downloads a single file.
#' @note Could recode to use \code{\pkg{curl}} package, since curl::curl_download() is a replacement for base download.file() with better performance,
#'   support for encryption (https, ftps), gzip compression, authentication, etc.
#' @param url The url of folder with files to download, as character string, or a vector:
#'   If files is specified, url should be the one folder without the filename. Otherwise, a vector of full paths with filenames.
#' @param files Optional. A character vector of file names to be found at url. If missing, assumes url is full path including filename.
#' @param destfiles Optional. A character vector of one or more file names. If missing, it uses same names as in files at url.
#' @param todir The folder where downloaded files will be placed, as a character string.
#' @param silent Logical, optional, FALSE by default. Prints a message using cat() if TRUE.
#' @param overwrite Optional, logical, FALSE by default. If FALSE, checks to see if file already exists in local folder and does not download if already exists.
#'   But note that may cause problems if zero size file exists already due to earlier failed download.
#' @param ... optional parameters passed to download.file
#' @return Returns vector of numbers, each being 1 or 0 or 2 to signify success or failure or no attempt because file already seems to exist locally.
#' @seealso [download.file()] [curl::curl_download()]
#' @export
download.files <- function(url, files, destfiles, todir, silent=FALSE, overwrite=FALSE, ...) {

  urls <- url # confusing to use url since there is a function url() in base
  #urls <- normalizePath(urls)

  downloadDone <- vector()
  # *** SOME NETWORKS HAVE FIREWALL RULES THAT DO NOT ALLOW R TO DOWNLOAD ZIP FILES VIA SOFTWARE LIKE THIS:

  if (missing(files)) {
    # assume urls includes files as part of the path
    files <- basename(urls)
    for (i in 1:length(files)) {
      urls[i] <- gsub(pattern = files[i], replacement = '', urls[i])
    }
    stop('multiple urls not implemented yet')
  }

  if (missing(destfiles)) {
    destfiles <- files
  }

  for (i in 1:length(files)) {
    if (!(file.exists(file.path(todir, destfiles[i])))) {
      if (!file.exists(file.path(urls, files[i]))) {
        warning(paste('Cannot find local or remote copy of ', file.path(urls, files[i]), '\nnor\n', file.path(urls, files[i])) )
        downloadDone[i] <- 0
      } else {
        downloadDone[i] <- download.file( file.path(urls, files[i]), file.path(todir, destfiles[i]), ...)
        pause(3)
      }
    } else {
      if (overwrite) {
        # already have local copy but want to overwrite (but note did not check if exists remotely)
        downloadDone[i] <- download.file( file.path(urls, files[i]), file.path(todir, destfiles[i]), ...)
        pause(3)
      } else {
        # already have local copy and do not want to overwrite
        downloadDone[i] <- 2
      }
    }
  }

  if (!silent) {
    cat('Checked: ', length(files), '\n')
    cat('  Already exist locally and do not want to overwrite: ', sum(downloadDone == 2), '\n')
    cat('  Tried to download: ', sum(downloadDone != 2), '\n')
    cat('    Downloaded successfully: ', sum(downloadDone == 1), '\n')
    cat('    Downloaded error: ', sum(downloadDone == 0), '\n')
    cat('Errors: \n')
    print(cbind(files[downloadDone == 0]))
  }
  return(downloadDone)
}


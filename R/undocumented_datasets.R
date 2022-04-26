#' which data files in /data folder lack .R files in /R folder?
#'
#' @param pkgfolder base folder of the package, default is current working directory
#' @param verbose whether to print to console all data files (without extension)
#'
#' @export
#'
undocumented_datasets <- function(pkgfolder=getwd(), verbose=FALSE) {
  # just lists the .rda kinds of files in ./data that lack matching .R files in /R
  rdfiles = paste0(gsub(pattern = '\\.rda?t?a?$', '', list.files(file.path(pkgfolder, 'data')), ignore.case = TRUE),'.R')
  if (verbose) {print( cbind(gsub('\\.R$','',rdfiles), rdfiles %in% list.files('./R'))); cat('\n')}
  rdfiles[!(rdfiles %in% list.files(file.path(pkgfolder, 'R')))]
}

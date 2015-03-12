#' @title Help removing all objects from memory
#' 
#' @description A simple way to get a reminder of how to clear all objects from memory because I always forget how
#' @return prints how to do that
#' @export
rmall <- function() {
  cat('type in this:\n')
  cat('rm(list=ls())')
}

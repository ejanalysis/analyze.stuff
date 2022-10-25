#' @title Get the arguments of a function
#' @description Get the arguments of a function as shown in help for [grep()]
#' @param env e.g., 'package:analyze.stuff' 
#' @param pattern search query as regular expression
#' @return arguments
#' @examples
#'  findArgs("package:base", "warn")
#' @export
findArgs <- function(env, pattern) {
  # code is from examples in  ?grep
  nms <- ls(envir = as.environment(env))
  nms <- nms[is.na(match(nms, c("F","T")))] # <-- work around "checking hack"
  aa <- sapply(nms, function(.) { o <- get(.)
  if(is.function(o)) names(formals(o)) })
  iw <- sapply(aa, function(a) any(grepl(pattern, a, ignore.case=TRUE)))
  aa[iw]
}

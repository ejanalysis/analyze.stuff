#' @title Format a table in roxygen documentation of function in a package
#' @description modified version of func in help section on formatting in \pkg{roxygen2} package
#' @param df data.frame required
#' @param ... optional parameters passed through to lapply(df, format, ...)
#' @return Returns text that can be pasted into documentation of a function or data in a package
#' @seealso Help on formatting in \pkg{roxygen2}
#' @examples
#'   tabular(mtcars[1:5, 1:5])
#'   tabular(df = data.frame(a=7:16, b='stuff', c=999, d=c('blah','junk')) )
#' @export
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n#'  ")))

  cat( paste("#' \\tabular{", paste(col_align, collapse = ""), "}{\n#'  ",
        contents, "\n#' }\n", sep = "")
  )
}

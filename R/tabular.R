#' @title Format a table in roxygen documentation of function in a package
#'
#' @description taken from \code{\link[roxygen2]{formatting}} formatting \url{http://127.0.0.1:31032/help/library/roxygen2/doc/formatting.html}
#' @param df data.frame required
#' @param ... optional parameters passed through to lapply(df, format, ...)
#' @return Returns text that can be pasted into documentation of a function or data in a package
#' @seealso \code{\link[roxygen2]{formatting}}
#' @examples
#'   # cat(tabular(mtcars[1:5, 1:5]))
#' @export
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
        contents, "\n}\n", sep = "")
}

tabular <- function(df, ...) {
  
  # taken from ??formatting 
  #http://127.0.0.1:31032/help/library/roxygen2/doc/formatting.html
  
  # helps format a table for roxygen documentation of function
  
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  
  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))
  
  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
        contents, "\n}\n", sep = "")
  
  # cat(tabular(mtcars[1:5, 1:5]))
}



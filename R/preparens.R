preparens <- function() {
  # was trying to have a shortcut that could make it easier to type something and then put parens around it?
    selected <- rstudioapi::primary_selection(rstudioapi::getSourceEditorContext()$selection)
    rstudioapi::modifyRange(location = selected$range, text = paste0('(', selected$text, ')'))
    rstudioapi::setCursorPosition(selected$range$start)
}

preparens <- function() {
    selected <- rstudioapi::primary_selection(rstudioapi::getSourceEditorContext()$selection)
    rstudioapi::modifyRange(location = selected$range, text = paste0('(', selected$text, ')'))
    rstudioapi::setCursorPosition(selected$range$start)
}

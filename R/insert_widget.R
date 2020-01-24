#' Inserts a widget
#' @description Wrapper around insertUI(widgetUI(...)) and callModule(widget, ...).
#'@export
insert_widget <- function(id, args, datasets, 
                          selector = "#placeholder", where = "beforeEnd",
                          buttons = c("close","edit")){
  insertUI(selector, where = where, 
           ui = widgetUI(id, args = args, datasets = datasets, buttons = buttons))
  
  callModule(widget, id, args = args, datasets = datasets, id_copy = id)
  
}
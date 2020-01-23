#' Inserts a widget
#' @description Wrapper around insertUI(widgetUI(...)) and callModule(widget, ...).
#'@export
insert_widget <- function(id, args, datasets, selector = "#placeholder", where = "beforeEnd"){
  insertUI(selector, where = where, 
           ui = widgetUI(id, args = args, datasets = datasets))
  
  callModule(widget, id, args = args, datasets = datasets, id_copy = id)
  
}
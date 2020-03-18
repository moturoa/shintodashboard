#' Inserts a widget
#' @description Wrapper around insertUI(widgetUI(...)) and callModule(widget, ...).
#'@export
insert_widget <- function(id, 
                          args, 
                          datasets, 
                          selector = "#placeholder", 
                          where = "beforeEnd",
                          buttons = c("close","edit"),
                          size = list(width = 500, height = 450, 
                                      margin = 10, padding = 25, 
                                      padding_bottom = 100)
                          ){
  
  ui <- widgetUI(id, args = args, 
                 datasets = datasets, 
                 buttons = buttons,
                 widget_size = size)
  
  insertUI(selector, where = where, ui = ui)
  
  callModule(widget, 
             id, 
             args = args, 
             datasets = datasets, 
             id_copy = id)
  
}

#' Loads all widgets from a JSON
#'@export
insert_saved_widgets <- function(dash, datasets, buttons = "", ...){
  
  if(is.character(dash))dash <- jsonlite::fromJSON(dash)

  settings <- list()  

  for(i in seq_along(dash)){
    new_id <- uuid::UUIDgenerate()
    insert_widget(new_id, dash[[i]], datasets, buttons = buttons, ...)
    settings[[new_id]] <- dash[[i]]
  }
  
return(settings)
}

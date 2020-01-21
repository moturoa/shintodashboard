#' Shiny module om een kolom te filteren
#' @rdname columnFilter
#' @export
columnFilterUI <- function(id, data, preset = NULL){
  
  col_select <- if(is.null(preset))NULL else preset$column
  
  ns <- NS(id)
  
  tags$div(id = id, 
    tagList(
      side_by_side(
        selectInput(ns("select_column"), 
                    "Filter deze kolom", 
                    choices = names(data),
                    selected = col_select),
        uiOutput(ns("column_filter_ui")),
        vertical_align = TRUE
      ),
      tags$br()
    )
  )
}

#' @rdname columnFilter
#' @export
columnFilter <- function(input, output, session, data, preset = NULL){
  
  out <- reactiveValues(
    column = NULL,
    class = NULL,
    value = NULL
  )
  
  columnData <- reactive({
    
    req(input$select_column)
    column <- data[[input$select_column]]
    if(is.factor(column))column <- as.character(column)
    
  return(column)
  })
  
  observeEvent(input$select_column, {
    
    column <- columnData()
    col_class <- class(column)
    
    if(col_class == "numeric"){
      
      if(!is.null(preset)){
        vals <- preset$value
      } else {
        vals <- c(min(column, na.rm = TRUE), 
                  max(column, na.rm = TRUE))
      }
      
      ui <- numericRangeInput(session$ns("column_filter"), "Min / Max",
                        value = vals)
    }
    
    if(col_class == "Date"){
      
      if(!is.null(preset)){
        vals <- preset$value
      } else {
        vals <- c(min(column, na.rm = TRUE), 
                  max(column, na.rm = TRUE))
      }
      
      ui <- dateRangeInput(session$ns("column_filter"), "Min / Max",
                     start = vals[1],  end = vals[2])
      
    }
    
    if(col_class %in% c("character","factor")){
      
      if(!is.null(preset)){
        vals <- preset$value
      } else {
        vals <- unique(column)
      }
      
      ui <- pickerInput(session$ns("column_filter"), "Selecteer", 
                        choices = unique(column), 
                        selected = vals,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE,
                                       `deselect-all-text` = "Alles uit",
                                       `select-all-text` = "Alles aan"))
    }
    
    output$column_filter_ui <- renderUI(ui)
    
    
  })
  
  observe({
    
    out$column <- input$select_column
    out$value <- input$column_filter
    out$class <- class(columnData())
    
  })
  
return(out)
}


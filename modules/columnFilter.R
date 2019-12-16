
columnFilterUI <- function(id, data){
  
  
  ns <- NS(id)
  
  tagList(
    side_by_side(
      selectInput(ns("select_column"), "Filter deze kolom", choices = names(data)),
      uiOutput(ns("column_filter_ui")),
      vertical_align = TRUE
    ),
    tags$br()
  )
  
}

columnFilter <- function(input, output, session, data){
  
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
    
    ui <- switch(col_class, 
           
           numeric = numericRangeInput(session$ns("column_filter"), "Min / Max",
                                       value = c(min(column, na.rm = TRUE),
                                                 max(column, na.rm = TRUE))),
           Date = dateRangeInput(session$ns("column_filter"), "Min / Max",
                                       start = min(column, na.rm = TRUE),
                                       end = max(column, na.rm = TRUE)),
           character = selectInput(session$ns("column_filter"), "Selecteer", choices = unique(column), multiple = TRUE)
           )
    
    output$column_filter_ui <- renderUI(ui)
    
    
  })
  
  observe({
    
    out$column <- input$select_column
    out$value <- input$column_filter
    out$class <- class(columnData())
    
  })
  
return(out)
}


# h4(label),
#           selectInput(session$ns(glue("{idbase}3")), "", 
#                       width = 300,
#                       choices = sort(unique(data)), multiple=TRUE)
#         )
#         
#       } else {
#         
#         if(is.numeric(data)){
#           
#           el <- tagList(
#             h4(label),
#             side_by_side(
#               numericInput(session$ns(glue("{idbase}1")), "min", value=min(data), width = 100),
#               numericInput(session$ns(glue("{idbase}2")), "max", value=max(data), width = 100)
#             ),
#             br()
#           ) 
#           
#         }
#         
#         if(inherits(data, "Date")){
#           
#           el <- tagList(
#             h4(label),
#             dateRangeInput(session$ns(glue("{idbase}4")), "", 
#                            start = min(data), 
#                            end = max(data),
#                            width = 300,
#                            format = "dd/mm/yy", 
#                            language = "nl")
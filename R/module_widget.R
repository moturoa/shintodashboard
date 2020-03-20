
widgetUI <- function(id, args, datasets, buttons = c("close","edit"), 
                     widget_size = list(width = 500, height = 450, 
                                        margin = 10, padding = 25, 
                                        padding_bottom = 100)){
  
  ns <- NS(id)
  
  # Find dataset based on saved setting, OR, if only one dataset provided, use it.
  if(length(datasets) > 1){
    dataset <- datasets[[args$dataset]]  
  } else {
    dataset <- datasets[[1]]
  }
  
  
  # Twee knopjes, 1 plotOutput.
  inner_content <- list(
    if("close" %in% buttons){
      actionButton(ns("btn_close"), 
                 label = HTML("&times;"), class = "plotbutton")
    },
    if("edit" %in% buttons){
      actionButton(ns("btn_edit"), 
                   label = "", icon = icon("edit"), 
                   class = "plotbutton"
      )  
    },
    plotOutput(ns("widget_plot"), 
               width = widget_size$width - widget_size$padding*2, 
               height = widget_size$height - widget_size$padding - widget_size$padding_bottom)
  )
  
  
  if(!(is.null(args$interactive) || args$interactive$nelements == 0)){
    
    make_interactive_element <- function(i){
      
      if(i > args$interactive$nelements){
        return(NULL)
      }
      
      varlab <- paste0("variable",i)
      ellab <- paste0("element",i)
      label <- paste0("label",i)
      value <- args$interactive_vals[[i]]
      column_data <- dataset[, args$interactive[[varlab]]]

      if(args$interactive[[ellab]] == "selectInput"){
        el <- shinyWidgets::pickerInput(ns(paste0("interactive_", i)), 
                                        label = args$interactive[[label]],
                                        choices = sort(unique(column_data)),
                                        selected = if(is_empty(value)) unique(column_data) else value,
                                        multiple = TRUE,
                                        options = list(`actions-box` = TRUE,
                                                       `selected-text-format` = "count > 3"),
                                        width = "200px"
        )
      } else if(args$interactive[[ellab]] == "sliderInput"){
        el <- shiny::sliderInput(ns(paste0("interactive_", i)),
                                 label = args$interactive[[label]],
                                 sep = "",
                                 min = min(column_data, na.rm=TRUE),
                                 max = max(column_data, na.rm=TRUE),
                                 value = if(is_empty(value))c(min(column_data, na.rm=TRUE), max(column_data, na.rm=TRUE)) else value,
                                 width = "200px"
        )
      } else if(args$interactive[[ellab]] == "dateRangeInput") {
        el <- shiny::dateRangeInput(ns(paste0("interactive_", i)),
                                    label = args$interactive[[label]],
                                    min = min(column_data, na.rm=TRUE),
                                    max = max(column_data, na.rm=TRUE),
                                    start = if(is_empty(value))min(column_data, na.rm=TRUE) else value[1],
                                    end = if(is_empty(value))max(column_data, na.rm=TRUE) else value[2],
                                    width = "200px"
        )
      }
      
      return(el)
    }
    
    inner_content <- c(inner_content, list(
      side_by_side(
        make_interactive_element(1),
        tags$div(style = "width:20px;"),
        make_interactive_element(2),
        vertical_align = TRUE
      )
    ))

  }
  
  out <- withTags(
    
    div(id = ns("container"), class = "customplot cpbox", 
        style = glue("width: {widget_size$width}px;",
                     "height: {widget_size$height}px;",
                     "margin: {widget_size$margin}px;",
                     "vertical-align: top;",
                     #"padding: {widget_size$padding}px {widget_size$padding}px {widget_size$padding_bottom}px {widget_size$padding}px;",
                     "display: inline-block;"),
        
        div(class = "box-body", 
            inner_content
        )
        
    )
    
  )
  
return(out) 
}
  


widget <- function(input, output, session, id_copy, args, datasets){
  
  

  # Find dataset based on saved setting, OR, if only one dataset provided, use it.
  if(length(datasets) > 1){
    dataset <- datasets[[args$dataset]]  
  } else {
    dataset <- datasets[[1]]
  }
  
  # Set values for the interactive elements
  interactive_vals <- reactive(list(input$interactive_1, input$interactive_2))
  
  # Render plot
  output$widget_plot <- renderPlot({

    custom_plot(plotarguments = args,
                data = dataset,
                interactive = interactive_vals())
    
  })

  
  # Close button
  observeEvent(input$btn_close, {

    removeUI(selector = paste0("#", session$ns("container")), session = session)

  })
  
  # Edit button
  observeEvent(input$btn_edit, {
    print(id_copy)
    session$userData$plotedit$id <- id_copy
    session$userData$plotedit$ping <- runif(1)

  })

}
  
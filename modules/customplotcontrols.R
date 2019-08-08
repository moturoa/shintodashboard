
customplotcontrolsUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(id = "panel_controls",
              fluidRow(
                column(3,
                       varSelectizeInput(ns("plot_xvar"), label = "X-as variabele", 
                                         data = automobiles, selected = "engine_volume"),
                       varSelectizeInput(ns("plot_yvar"), label = "Y-as variabele", 
                                         data = automobiles, selected = "fuel_efficiency"),
                       checkboxInput(ns("chk_usegroup"), "Gebruik groep"),
                       varSelectizeInput(ns("plot_groupvar"), label = "Groep variabele", 
                                         data = automobiles, selected = "cylinders")
                       
                ),
                column(3, 
                       
                       selectInput(ns("plot_type"), "Plot type", 
                                   choices = c("Scatter", "Barplot", "Stacked barplot")),
                       tags$p("Voor barplots select functie toe te passen op X en/of group variabele"),
                       selectInput(ns("plot_stat"), "Functie", 
                                   choices = c("mean","count","max", "sum"))
                       
                ),
                column(3,
                       
                       textInput(ns("plot_xlab"), "X-as label"),
                       textInput(ns("plot_ylab"), "Y-as label"),
                       textInput(ns("plot_glab"), "Groep label")
                       
                ),
                column(3,
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       actionButton(ns("btn_addplot"), "Add plot"),
                       actionButton(ns("btn_reset"), "Reset", icon=icon("refresh"))
                )
                
              )
    ),
    fluidRow(
      div(id="placeholder")
    )
  )
}


customplotcontrols <- function(input, output, session){
  
  rv <- reactiveValues(
    n_added = 1
  )
  
  observeEvent(input$btn_addplot, {
    
    id_ <- paste0("customplot", rv$n_added)
    
    insertUI(
      "#placeholder", where = "beforeEnd",
      ui = customplotUI(id_)
    )
    
    make_null <- function(x){
      if(x == "")x <- NULL
      x
    }
    
    callModule(customplot, id_, this_id = id_, session = session,
               
               plot_arguments = list(
                 plottype = as.character(input$plot_type),
                 xvar = as.character(input$plot_xvar), 
                 yvar = as.character(input$plot_yvar),
                 groupvar = if(input$chk_usegroup)as.character(input$plot_groupvar) else NULL,
                 xlab = make_null(input$plot_xlab),
                 ylab = make_null(input$plot_ylab),
                 glab = make_null(input$plot_glab),
                 statfun = as.character(input$plot_stat)
               )
    )
    
    rv$n_added <- rv$n_added + 1
    
  })
  
  observeEvent(input$btn_reset, {
    shinyjs::reset("panel_controls")
  })
  
}


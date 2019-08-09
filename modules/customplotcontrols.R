
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
    textOutput(ns("allids")),
    fluidRow(
      div(id="placeholder")
    )
  )
}


customplotcontrols <- function(input, output, session){
  
  jqui_sortable('#placeholder', options = list(opacity = 0.5))
  
  rv <- reactiveValues(
    n_added = 1,
    all_ids = NULL
  )
  
  ns <- session$ns
  
  output$allids <- renderText({
    paste(rv$all_ids)
  })
  
  observeEvent(input$btn_reset, {
    shinyjs::reset("panel_controls")
  })
  
  observeEvent(input$btn_addplot, {
    
    id_container <- ns(paste0("customplot", rv$n_added))
    id_plot <- paste0(id_container, "_plot")
    id_closebutton <- paste0(id_container,"_btn_close")
    id_editbutton <- paste0(id_container,"_btn_edit")
    id_downloadbutton <- paste0(id_container,"_btn_download")
    
    insertUI(
      "#placeholder", where = "beforeEnd",
      
      tags$div(class = "col-sm-4", id = id_container,
               tags$div(class = "box cpbox",
                        tags$div(class = "box-body",
                                 actionButton(ns(id_closebutton), 
                                              label=HTML("&times;"), class="plotbutton"),
                                 actionButton(ns(id_editbutton), 
                                              label="", icon=icon("edit"), class="plotbutton"),
                                 actionButton(ns(id_downloadbutton), 
                                              label="", icon=icon("download"), class="plotbutton"),
                                 plotOutput(ns(id_plot))
                        )
               )
      )
    )
    
    rv$all_ids <- c(rv$all_ids, id_container)
    
    make_null <- function(x){
      if(x == "")x <- NULL
      x
    }
    
    output[[id_plot]] <- renderPlot({
    
      isolate(
        custom_plot(plot_arguments = list(
          plottype = as.character(input$plot_type),
          xvar = as.character(input$plot_xvar), 
          yvar = as.character(input$plot_yvar),
          groupvar = if(input$chk_usegroup)as.character(input$plot_groupvar) else NULL,
          xlab = make_null(input$plot_xlab),
          ylab = make_null(input$plot_ylab),
          glab = make_null(input$plot_glab),
          statfun = as.character(input$plot_stat)
        ))
      )
    }, width = 380, height = 280)
    
    
    observeEvent(input[[id_closebutton]], {
      
      removeUI(selector = paste0("#", id_container), session = session)
      rv$all_ids <- rv$all_ids[-match(id_container, rv$all_ids)]
      
    })
    
    observeEvent(input[[id_editbutton]], {
      
      output[[id_plot]] <- renderPlot({
        
        isolate(
          custom_plot(plot_arguments = list(
            plottype = as.character(input$plot_type),
            xvar = as.character(input$plot_xvar), 
            yvar = as.character(input$plot_yvar),
            groupvar = if(input$chk_usegroup)as.character(input$plot_groupvar) else NULL,
            xlab = make_null(input$plot_xlab),
            ylab = make_null(input$plot_ylab),
            glab = make_null(input$plot_glab),
            statfun = as.character(input$plot_stat)
          ))
        )
      }, width = 380, height = 280)
      
    }) 
    
    
    rv$n_added <- rv$n_added + 1
    
  })
  

  
}


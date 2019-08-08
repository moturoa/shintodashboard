

source("global.R")

customplotUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(class = "col-sm-4", id = id,
           tags$div(class = "box cpbox",
                    tags$div(class = "box-body",
                        actionButton(ns("btn_close"), label=HTML("&times;"), class="closebox"),
                        actionButton(ns("btn_edit"), label="", icon=icon("edit"), class="closebox"),
                        actionButton(ns("btn_download"), label="", icon=icon("download"), class="closebox"),
                        plotOutput(ns("customplot"))
                    )
           )
  )
  
}


customplot <- function(input, output, session, this_id, 
                       plot_arguments
                       ){
  
  output$customplot <- renderPlot({
    
    a <- plot_arguments
    if(a$plottype == "Scatter"){
      p <- scatter_plot(automobiles, 
                        a$xvar, 
                        a$yvar, 
                        a$groupvar, 
                        xlab=a$xlab, 
                        ylab=a$ylab, 
                        glab=a$glab)
      print(p)
    }
    if(a$plottype == "Barplot"){
      p <- grouped_barplot(automobiles, a$xvar, a$yvar, a$groupvar, 
                           statfun=a$statfun,
                           xlab=a$xlab, 
                           ylab=a$ylab, glab=a$glab)
      print(p)
    }
    if(a$plottype == "Stacked barplot"){
      p <- grouped_barplot(automobiles, a$xvar, a$yvar, a$groupvar, 
                           statfun=a$statfun,
                           xlab=a$xlab, ylab=a$ylab, glab=a$glab,
                      position = "stacked")
      print(p)
    }
    
    
  }, width = 380, height = 280)
    
  observeEvent(input$btn_close, {
    
      removeUI(selector = paste0("#", this_id), session = session)
    
  })
  
}


ui <- fluidPage(
  includeCSS("www/style.css"),
  
  wellPanel(
   fluidRow(
    column(3,
      varSelectizeInput("plot_xvar", label = "X-as variabele", 
                  data = automobiles, selected = "engine_volume"),
      varSelectizeInput("plot_yvar", label = "Y-as variabele", 
                        data = automobiles, selected = "fuel_efficiency"),
      checkboxInput("chk_usegroup", "Gebruik groep"),
      varSelectizeInput("plot_groupvar", label = "Groep variabele", 
                        data = automobiles, selected = "cylinders")
      
    ),
    column(3, 
    
           selectInput("plot_type", "Plot type", 
                       choices = c("Scatter", "Barplot", "Stacked barplot")),
           tags$p("Voor barplots select functie toe te passen op X en/of group variabele"),
           selectInput("plot_stat", "Functie", 
                       choices = c("mean","count","max", "sum"))
           
           ),
    column(3,
    
           textInput("plot_xlab", "X-as label"),
           textInput("plot_ylab", "Y-as label"),
           textInput("plot_glab", "Groep label")
                  
    ),
    column(3,
           tags$br(),
           tags$br(),
           tags$br(),
           actionButton("btn_addplot", "Add plot")
    )
   )
  ),
  
  fluidRow(
     div(id="placeholder")
  )
  
)

server <- function(input, output, session){
  
  
  rv <- reactiveValues(
    n_added = 1
  )
  
  # Auto-stop app on window close
  session$onSessionEnded(stopApp)
  
  
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
  
  
}



shinyApp(ui = ui, 
         server = server)



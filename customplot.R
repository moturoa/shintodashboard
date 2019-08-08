




library(shiny)

customplotUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(class = "col-sm-4", id = id,
           tags$div(class = "box cpbox",
                    tags$div(class = "box-body",
                        actionButton(ns("close"), label=HTML("&times;"), class="closebox"),
                        plotOutput(ns("customplot"))
                    )
           )
  )
  
}


customplot <- function(input, output, session, this_id, xvar, yvar, pch=19){
  
  observe({
    
    x <- mtcars[,xvar]
    y <- mtcars[,yvar]
    
    output$customplot <- renderPlot({
      plot(x,y, xlab=xvar, ylab=yvar, pch=pch)
    }, width = 250, height = 250)
    
  })
  
  observeEvent(input$close, {

        removeUI(selector = paste0("#", this_id), session = session)
  
  })
  
}


ui <- fluidPage(
  includeCSS("www/style.css"),
  
  wellPanel(
    
    selectInput("xvar_newplot", label = "X variable", 
                choices =  names(mtcars), selected = "wt"),
    selectInput("yvar_newplot", label = "Y variable", 
                choices =  names(mtcars), selected = "mpg"),
    actionButton("btn_addplot", "Add plot")
      
  ),
  
  textOutput("btnclicked"),
  fluidRow(
     div(id="placeholder")
  )
  
)

server <- function(input, output, session){
  
  
  rv <- reactiveValues(
    nclicked = 1
  )
  
  # Auto-stop app on window close
  session$onSessionEnded(stopApp)
  
  observeEvent(input$btn_addplot, {
    
    id_ <- paste0("customplot", rv$nclicked)
    
    insertUI(
      "#placeholder", where = "beforeEnd",
      ui = customplotUI(id_)
    )
    
    callModule(customplot, id_, this_id = id_, session = session,
               xvar = input$xvar_newplot, yvar = input$yvar_newplot)
    
    rv$nclicked <- rv$nclicked + 1
    
  })
  
  
}



shinyApp(ui = ui, 
         server = server)



library(shinydashboard)
library(shinydashboardPlus)

library(ggplot2)
library(lgrdata)
data(automobiles)

library(shiny)
library(dplyr)
library(glue)
library(shinyjs)
library(shinyjqui)
library(shinydashboard)
library(plotly)
library(RColorBrewer)
library(ggthemes)

source("../R/plot_wrappers.R")
source("../R/functions.R")


# Config
dash <- readRDS("../cache/dashboards.rds")[["wbm2"]]
widget_size <- list(width = 500, height = 400, margin = 10, padding = 25)



ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      includeCSS("www/style.css"),
      
    uiOutput("static_dashboard")
      
    )
)
  
server <- function(input, output, session) {
    
  session$onSessionEnded(stopApp)
  
  output$static_dashboard <- renderUI({
    
    add_plot <- function(plotarguments){
    
      id_container <- paste0("customplot", random_word(6))
      id_plot <- paste0(id_container, "_plot")

      out <- withTags(
          div(class = "cpbox", style = glue("width: {widget_size$width}px; height: {widget_size$height}px;"),
             
              div(class = "cpbox-plot", style = glue("padding: {widget_size$padding}px"),
                 plotOutput(id_plot, 
                            width = widget_size$width - widget_size$padding*2, 
                            height = widget_size$height - widget_size$padding*2)
              )
             
            )
      )

      output[[id_plot]] <- renderPlot({
       
          custom_plot(plotarguments)
      
      })
      
    return(out)
    }
  
    
    plots <- lapply(seq_along(dash), function(i){
      add_plot(plotarguments = dash[[i]])
    })
    
    plots$cellArgs <- list(
      style = glue("
        width: auto;
        height: auto;
        margin: {widget_size$margin}px;
        "))
    
    do.call(flowLayout, plots)
    
  })
    
}


shinyApp(ui = ui, server = server)

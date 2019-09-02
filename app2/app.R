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

dash <- readRDS("../cache/dashboards.rds")[[1]]


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      fluidRow(
        div(id="placeholder")
      ) 
    )
)
  
server <- function(input, output, session) {
    session$onSessionEnded(stopApp)
  
  
    add_plot <- function(plotarguments){
    
      id_container <- paste0("customplot", random_word(6))
      id_plot <- paste0(id_container, "_plot")

      insertUI(
        "#placeholder", where = "beforeEnd",
        
        withTags(
          div(id = id_container,  class = "col-sm-4", 
                 div(class = "box cpbox",
                        div(class = "box-body",
                             plotOutput(id_plot)
                          )
                 )
          )
        )
        
      )
      
      output[[id_plot]] <- renderPlot({
        
        print(plotarguments$plottype)
        custom_plot(plotarguments)
        
      }, height = 280)
      
    }
  
    
    lapply(seq_along(dash), function(i){
      add_plot(plotarguments = dash[[i]])
    })
    
}


shinyApp(ui = ui, server = server)

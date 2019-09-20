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

library(sf)
library(leaflet)

source("../R/plot_wrappers.R")
source("../R/functions.R")
source("R/dashboardheader_modified.R")
source("R/woningproductie_map.R")

woning_productie <- read.csv("../data/woningproductie_df.csv", stringsAsFactors = FALSE)

title.navbar.html <- tags$div(style="color:white;
                                    text-align:left;
                                    line-height: 50px;
                                    display:inline-block;
                                    font-size: 30px;
                                    font-size: 1.5vw;
                                    padding-left: 5px",
                              HTML(paste0("Woning productie")))

header <- dashboardHeader(
  title = img(src="ShintoLabs.png", height = 45),
  title.navbar = title.navbar.html
)

# Config
dash <- jsonlite::fromJSON("../cache/wbm2.json")
widget_size <- list(width = 500, height = 400, margin = 10, padding = 25)

gicon <- function(...)icon(..., lib = "glyphicon")

ui <- dashboardPage(
    header,
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", icon = gicon("dashboard"), tabName = ""),
        menuItem("Woningvoorraad", icon = gicon("home"), tabName = ""),
        menuItem("Woningproductie", icon = gicon("wrench"), tabName = ""),
        menuItem("Beheer project", icon = gicon("folder-open"), tabName = ""),
        menuItem("Rapportage", icon = gicon("stats"), tabName = ""),
        menuItem("Administratie", icon = gicon("cog"), tabName = "")
      ),
      textOutput("val1")
    ),
    dashboardBody(
      includeCSS("www/style.css"),
      
    uiOutput("static_dashboard"),
    uiOutput("map_preview")
      
    )
)
  
server <- function(input, output, session) {
    
  session$onSessionEnded(stopApp)
  
  output$static_dashboard <- renderUI({
    
    add_plot <- function(plotarguments){
    
      id_container <- paste0("customplot", random_word(6))
      id_plot <- paste0(id_container, "_plot")

      out <- withTags(
          div(class = "cpbox", 
              style = glue("width: {widget_size$width}px; height: {widget_size$height}px;"),
             
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
  
  output$map_preview <- renderUI({
    
    id_container <- paste0("customplot", random_word(6))
    id_plot <- paste0(id_container, "_plot")
    
    out <- withTags(
      div(class = "cpbox", 
          style = glue("width: {widget_size$width}px; height: {widget_size$height}px;"),
          
          div(class = "cpbox-plot", style = glue("padding: {widget_size$padding}px"),
             img(src = "zaanstad_map_preview.PNG", width = 450, height = 350,
                 onclick = 'Shiny.onInputChange("mapPreviewClick", Math.random());'
                 )
          )
          
      )
    )
    
    flowLayout(out, cellArgs = list(style = glue("
        width: auto;
        height: auto;
        margin: {widget_size$margin}px;
        ")))
  })

  observeEvent(input$mapPreviewClick, {
    
    md <- modalDialog(title="Woning productie",
                      footer = modalButton("Sluiten"),
                      size = "l",
                      easyClose = TRUE,
                      sliderInput("jaar_slide", "Selecteer jaartal woning levering",
                                  sep="",
                                  min=2017,max=2050,value=c(2017,2050)),
                      leafletOutput("modal_map", height = 700)
                      
                      )
    
    output$modal_map <- renderLeaflet({
      woning_productie_map(input$jaar_slide, woning_productie)
    })
    
    showModal(md)
    
  })
    
}


shinyApp(ui = ui, server = server)

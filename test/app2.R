library(shiny)


library(DT)
library(shinyWidgets)
library(glue)
library(shinyjqui)
library(shinyjs)
library(shinydashboard)

fns <- dir("../R", full.names=TRUE)
for(z in fns)source(z)

library(RColorBrewer)
library(shintoplotwrappers)
library(ggplot2)
library(ggthemes)

.settings <- list()


# Datasets
datasets_key <- c(
  "Originele tabel" = "zawa_plancapaciteit_origineel.rds",    
  "Huur / Koop" = "zawa_plancapaciteit_naar_huurkoop.rds",   
  "Huur / Koop en Prijsklasse" = "zawa_plancapaciteit_naar_huurkoop_en_prijsklasse.rds",
  "Leverjaar" = "zawa_plancapaciteit_naar_leverjaar.rds",            
  "Woningtype" = "zawa_plancapaciteit_naar_woningtype.rds",
  "Zaanstad - Fasering" = "zaanstad_plancapaciteit_fasering.rds")                            

# Lees alle datasets.
datasets_paths <- file.path("data", datasets_key)
datasets_content <- lapply(datasets_paths, readRDS) %>% 
  setNames(datasets_key)

dash <- jsonlite::fromJSON("cache/zawa_20200107.json")


ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/style.css"),
  
  uiOutput("ui_controls"),
  
  tags$hr(),
  actionButton("browse","browser()"),
  actionButton("reset_settings", "Reset", icon = icon("refresh")),
  actionButton("make_plot", "Maak plot", icon = icon("plus")),
  tags$hr(),
  textOutput("test"),
  tags$hr(),
  
  jqui_sortable(
    tags$div(id = "placeholder")  
  )
  
)

server <- function(input, output, session) {
    
  observeEvent(input$browse, browser())
  
  w_edit <- reactiveVal()
  args <- reactiveVal()
  
  output$ui_controls <- renderUI({
    input$reset_settings
    customplotcontrolsUI("controls", args = w_edit(), data_key = datasets_key, datasets = datasets_content)
  })
  
  .settings <<- insert_saved_widgets(dash[1:2], datasets_content, buttons = c("edit","close"))
  
  observeEvent(input$make_plot, {
    
    req(out())
    new_id <- uuid::UUIDgenerate()
    
    insert_widget(new_id, out(), datasets_content, where = "afterBegin")
    .settings[[new_id]] <- out()
  })

  output$test <- renderText({
    session$userData$plotedit()
  })
  
  observe({
    
    edited <- session$userData$plotedit()
    req(edited)
    
    args(.settings[[edited]])
    w_edit(args())
    
  })
  
  
  out <- callModule(customplotcontrols, 
                    "controls", 
                    data_key = datasets_key, 
                    datasets = datasets_content)
  
  # Read current plot settings
  observeEvent(args(), {
    
    print("calling")
    out <- callModule(customplotcontrols, 
                      "controls", 
                      data_key = datasets_key, 
                      datasets = datasets_content,
                      args = args())  
  })
  
  
  
}

shinyApp(ui, server)


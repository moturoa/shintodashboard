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


d <- datasets_content[["zaanstad_plancapaciteit_fasering.rds"]]
d <- dplyr::filter(d, soortwoning != "werken")
d$soortwoning <- factor(d$soortwoning, levels = c("koopklasse 5 (>350.000)",
                                                  "koopklasse 4 (<285.000-350.000)",
                                                  "koopklasse 3 (<215.000-285.000)",
                                                  "koopklasse 2 (<185.000-215.000)",
                                                  "koopklasse 1 (<185.000)",
                                                  "koopklasse onbekend",
                                                  "huurklasse 3 (>900)",
                                                  "huurklasse 2 (<720-900)",
                                                  "huurklasse 1 (<720)",
                                                  "huurklasse 0 (<417)",
                                                  "huur/koop onbekend"
                                                  ))
datasets_content[["zaanstad_plancapaciteit_fasering.rds"]] <- d

#dash <- jsonlite::fromJSON("cache/zawa20200121_v3.json")

dash <- readRDS("cache/dash.rds")


widget_size <- list(width = 500, 
                    height = 450, 
                    margin = 10, 
                    padding = 25, 
                    padding_bottom = 100)


ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/style.css"),
  includeScript("www/plotsdashboard.js"),
  
  uiOutput("ui_controls"),
  
  tags$hr(),
  actionButton("browse","browser()"),
  actionButton("reset_settings", "Reset", icon = icon("refresh")),
  actionButton("make_plot", "Maak plot", icon = icon("plus")),
  actionButton("save_dash", "Save dashboard", icon = icon("save")),
  tags$hr(),
  textOutput("test"),
  tags$hr(),
  
  jqui_sortable(
    tagList(
      tags$div(id = "placeholder")    
    )
    
  )
  
)

server <- function(input, output, session) {
    
  observeEvent(input$browse, browser())
  
  w_edit <- reactiveVal()
  args <- reactiveVal()
  
  output$ui_controls <- renderUI({
    input$reset_settings
    customplotcontrolsUI("controls", 
                         args = w_edit(), 
                         data_key = datasets_key, 
                         datasets = datasets_content)
  })
  
  .settings <<- insert_saved_widgets(dash, 
                                     datasets_content, 
                                     buttons = c("edit","close"),
                                     size = widget_size)
  
  observeEvent(input$make_plot, {
    
    req(out())
    new_id <- uuid::UUIDgenerate()
    
    insert_widget(new_id, out(), datasets_content, where = "afterBegin")
    .settings[[new_id]] <<- out()
  })

  output$test <- renderText({
    session$userData$plotedit()
  })
  
  

  observeEvent(input$save_dash, {
    
    session$sendCustomMessage("plotsdashboard", 
                              list(shiny_id = session$ns("visibleplots"),
                                   placeholder = "placeholder"
                                   ))
    
  })
  

  observe({
    
    req(input$visibleplots)
    ids <- gsub("-container","", input$visibleplots)
    saveRDS(.settings[ids], glue("cache/dash.rds"))
    
    #{format(Sys.time(), '%Y-%m-%d-%H-%M')}
    
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




source("global.R")
source("R/functions.R")
source("modules/customplotcontrols.R")

ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/style.css"),
  
  customplotcontrolsUI("controls")
  
)

server <- function(input, output, session){
  
  # Auto-stop app on window close
  session$onSessionEnded(stopApp)
  
  out <- callModule(customplotcontrols, "controls")
  
}



shinyApp(ui = ui, 
         server = server)



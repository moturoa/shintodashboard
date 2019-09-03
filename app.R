

source("global.R")
source("R/functions.R")
source("R/tooltip.R")
source("modules/customplotcontrols.R")


ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/style.css"),
  includeScript("www/tooltip.js"),
  
  customplotcontrolsUI("controls")
  
)

server <- function(input, output, session){
  
  session$onSessionEnded(stopApp)
  
  out <- callModule(customplotcontrols, "controls")

}



shinyApp(ui = ui, 
         server = server)



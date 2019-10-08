
source("global.R")

ui <- fluidPage(
  
  useShinyjs(),
  includeCSS("www/style.css"),
  includeScript("www/tooltip.js"),
  includeScript("www/plotorder.js"),
  #includeScript("www/confirmclose.js"),
  
  customplotcontrolsUI("controls")
  
)

server <- function(input, output, session){
  
  session$onSessionEnded(stopApp)
  
  out <- callModule(customplotcontrols, "controls")

}


shinyApp(ui = ui, 
         server = server)







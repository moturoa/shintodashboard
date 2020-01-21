
suppressPackageStartupMessages({
  # remotes::install_bitbucket("shintolabs/shintoplotwrappers", 
  #                            auth_user="remko_duursma", 
  #                            password="<<MYPASSWORD>>")
  
  library(shintoplotwrappers)
  library(lifecycle)
  
  library(ggplot2)
  library(shiny)
  library(dplyr)
  library(glue)
  
  library(shinyjs)
  library(shinyjqui)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  
  library(DT)
  
  library(RColorBrewer)
  library(ggthemes)
  library(colourpicker)
  library(jsonlite)
})

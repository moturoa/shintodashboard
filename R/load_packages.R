
suppressPackageStartupMessages({
  # remotes::install_bitbucket("shintolabs/shintoplotwrappers", 
  #                            auth_user="remko_duursma", 
  #                            password="<<MYPASSWORD>>")
  library(shinyfilters)
  library(shintoplotwrappers)
  
  library(ggplot2)
  library(shiny)
  library(dplyr)
  library(glue)
  library(shinyjs)
  library(shinyjqui)
  library(shinydashboard)
  library(shinyWidgets)
  library(RColorBrewer)
  library(ggthemes)
  library(colourpicker)
})

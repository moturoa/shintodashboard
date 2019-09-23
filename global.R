

# remotes::install_bitbucket("shintolabs/shintoplotwrappers", 
#                            auth_user="remko_duursma", 
#                            password="<<MYPASSWORD>>")
library(shintoplotwrappers)

library(ggplot2)
library(lgrdata)
library(shiny)
library(dplyr)
library(glue)
library(shinyjs)
library(shinyjqui)
library(shinydashboard)
# library(plotly)
library(RColorBrewer)
library(ggthemes)
library(colourpicker)

data(automobiles)
woning_productie <- read.csv("data/woningproductie_df.csv", stringsAsFactors = FALSE)


current_ids <- c()
plot_settings <- list()

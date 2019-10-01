

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

available_rds <- c("primavera_fasering_20191030.rds", 
	                     "primavera_overzicht_20191030.rds")

available_datasets <- tools::file_path_sans_ext(basename(available_rds))

for(i in seq_along(available_rds)){
	assign(available_datasets[i], readRDS(file.path("data", available_rds[i])))
}


current_ids <- c()
plot_settings <- list()




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
#woning_productie <- read.csv("data/woningproductie_df.csv", stringsAsFactors = FALSE)


ds <- c("primavera_huurkoop.rds","primavera_lever172819.rds","primavera_leverjaar.rds","primavera_prijs.rds",
        "primavera_startbouw.rds","primavera_woningtype.rds")
for(i in seq_along(ds)){
  assign(tools::file_path_sans_ext(basename(ds[i])), 
         readRDS(file.path("data", ds[i])))
}

available_datasets <- tools::file_path_sans_ext(basename(ds))



current_ids <- c()
plot_settings <- list()


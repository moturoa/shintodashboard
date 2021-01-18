
library(shintodashboard)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinyjs)
library(shinyjqui)

if(!dir.exists("cache"))dir.create("cache")
if(!dir.exists("cache/palettes"))dir.create("cache/palettes")

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


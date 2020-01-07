

source("R/load_packages.R")
source("R/functions.R")
source("R/functions_ui.R")
source("R/tooltip.R")
source("modules/customplotcontrols_ui.R")
source("modules/customplotcontrols_server.R")
source("modules/columnFilter.R")

if(!dir.exists("cache"))dir.create("cache")
if(!dir.exists("cache/palettes"))dir.create("cache/palettes")

# Datasets
datasets_key <- c(
  "Originele tabel" = "zawa_plancapaciteit_origineel.rds",    
  "Huur / Koop" = "zawa_plancapaciteit_naar_huurkoop.rds",   
  "Huur / Koop en Prijsklasse" = "zawa_plancapaciteit_naar_huurkoop_en_prijsklasse.rds",
  "Leverjaar" = "zawa_plancapaciteit_naar_leverjaar.rds",            
  "Woningtype" = "zawa_plancapaciteit_naar_woningtype.rds")                            
                         
# Lees alle datasets.
datasets_paths <- file.path("data", datasets_key)
datasets_content <- lapply(datasets_paths, readRDS) %>% 
  setNames(datasets_key)


# Global variables. Gebruikt om plot settings in op te slaan.
# (moet eigenlijk naar een reactiveval?)
current_ids <- c()
plot_settings <- list()


# Available color palettes
color_palettes <- c(tools::file_path_sans_ext(dir("cache/palettes", pattern = "[.]json$")), 
                        "rich.colors", rownames(brewer.pal.info))

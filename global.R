
library(ggplot2)
library(lgrdata)
library(shiny)
library(dplyr)
library(glue)
library(shinyjs)
library(shinyjqui)
library(shinydashboard)
library(plotly)
library(RColorBrewer)
library(ggthemes)
library(colourpicker)

data(automobiles)
woning_productie <- read.csv("data/woningproductie_df.csv")

source("R/plot_wrappers.R")


current_ids <- c()
plot_settings <- list()

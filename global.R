
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

source("R/plot_wrappers.R")


current_ids <- c()
plot_settings <- list()
dashboards <- list()


library(ggplot2)
library(lgrdata)
library(shiny)
library(dplyr)
library(glue)
library(shinyjs)
library(shinyjqui)

data(automobiles)

source("R/plot_wrappers2.R")


plot_settings <- list()
dashboards <- list()

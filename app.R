library('knitr')
library('ggplot2')
library('scales')
library('maps')
library('RColorBrewer')
library('countrycode', exclude = "select")
library('dplyr')
library('tidyverse')
library('naniar')

source("my_ui.R")
source("my_server.R")

shinyApp(ui = ui, server = server)


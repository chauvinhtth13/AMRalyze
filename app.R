library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
library(DT)

library(AMR)
library(tidyverse)
library(dplyr)
library(readxl)

source("ui/ui.R")
source("server/server.R")

shinyApp(ui = ui, server = server)
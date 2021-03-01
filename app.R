setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shinydashboard)
library(tidyverse)
library(lubridate)

# import data
source("load data.R")

# generate UI and server
source("ui.R", local = TRUE)
source("server.R")


shinyApp(ui, server)
# 12 Feb 2021
# Should scrape latest case information from Ministry of Health website, then
# tidy this data into suitable format for dashboard

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)


destfile <- "data/latest.csv"

current_date <- Sys.Date()
base_url <- "https://www.health.govt.nz/system/files/documents/pages/covid_cases_"

# Fetch CSV from URL
tryCatch({
  # Use todays date
  url <- paste(base_url, current_date, ".csv", sep = "")
  destfile <- paste("data/covid_cases_", current_date, ".csv", sep = "")
  download.file(url, destfile, mode = "wb")
},
warning=function(cond) {
  # Use yesterdays date
  url <- paste(base_url, current_date-1, ".csv", sep = "")
  destfile <- paste("data/covid_cases_", current_date-1, ".csv", sep = "")
  download.file(url, destfile, mode = "wb")
},
finally={
  message(paste("Processed_url:", url))
})


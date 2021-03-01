#' Load datasets used in the dashboard.

options(encoding = "UTF-8")

library(tidyverse)


## Importing and tidying NZ data -----------------------------------------------
# need to search folder for latest non-empty cases CSV
nz_filenames <- list.files("./data", 
                           pattern = "nz_covid_cases_\\d{4}-\\d{2}-\\d{2}.csv") %>%
  sort(decreasing = TRUE)

for (filename in nz_filenames) {
  raw_nz_cases <- read_csv(paste("./data/", filename, sep = ""),
                           col_types = cols())
  
  if (nrow(raw_nz_cases) != 0 & ncol(raw_nz_cases) != 0) {
    message(paste("Loading cases from:", filename))
    break
  }
  else if (filename == nz_filenames[length(nz_filenames)]) {
    stop("No non-empty NZ covid cases file")
  }
}

ages_order <- c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", 
                "50 to 59", "60 to 69", "70 to 79", "80 to 89", "90+")

nz_cases <- raw_nz_cases %>%
  rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
  mutate(report_date = as.Date(report_date, "%Y-%m-%d"),
         age_group = factor(age_group, levels = ages_order))


## Importing and tidying JHU global data ---------------------------------------
jhu_filenames <- list.files("./data", 
                            pattern = "JHU_global_cases\\d{4}-\\d{2}-\\d{2}.csv") %>%
  sort(decreasing = TRUE)

global_cases <- read_csv("data/JHU_global_cases_2021-02-17.csv",
                         col_types = cols()) %>%
  mutate(Country = factor(Country), Date = as.Date(Date, "%d/%m/%Y")) %>%
  mutate(Infection_Rate = Deaths / Confirmed)

# Infection rates of greater than 1 are not possible, and reflect mis-reporting
global_cases <- global_cases %>%
  mutate(Infection_Rate = if_else(Infection_Rate > 1, 1, Infection_Rate))
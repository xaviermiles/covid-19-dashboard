#' Imports and tidies global time series data for Covid-19 from JHU, exports    
#' final data frame as csv file.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)


datasets <- c(
  "confirmed_global",
  "recovered_global",
  "deaths_global"
)

base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"

df_longs = vector(mode = "list")

for (dataset in datasets) {
  source_url <- paste(base_url, dataset, ".csv", sep = "")
  df <- read_csv(source_url, col_types = cols())
  
  df_long <- df %>%
    pivot_longer(-(1:4), names_to = "Date", values_to = "count")
  
  if (dataset == "recovered_global") {
    df_long <- df_long %>%
      filter(`Country/Region` == "Canada") %>%
      mutate(`Province/State` = "All")
  }
  else {
    # Add count=NA entries for Country/Region=Canada, Province/State=All
    NA_rows <- df_long %>%
      filter(`Country/Region` == "Canada" & 
               `Province/State` == unique) %>%
      mutate(`Province/State` = "All", count = NA)
    
    print(NA_rows)
    
    df_long <- df_long %>%
      add_row(NA_rows)
  }
  
  df_longs[[dataset]] = df_long
}

ship_names <- c("Grand Princess", "Diamond Princess", "MS Zaandam")

global <- Reduce(
  function(...) left_join(..., 
                          by=c("Province/State", "Country/Region", "Lat", 
                               "Long", "Date"), 
                          all.x=TRUE), 
  df_longs
) %>%
  rename(Confirmed = count.x, Recovered = count.y, Deaths = count) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
  # filter(!(`Province/State` %in% ship_names | 
  #            `Country/Region` %in% ship_names)) %>% # remove ship data
  mutate(Active = Confirmed - Recovered - Deaths)
  
global_country <- global %>%
  group_by(`Country/Region`, Date) %>%
  summarise_at(c("Confirmed", "Deaths", "Recovered", "Active"), ~sum(.))

# Replace NA (in any column except Date) with 0
# full_table <- full_table %>% mutate_at(setdiff(names(full_table), "Date"), ~replace(., is.na(.), 0))

## Data Aggregation --------------------------------------------------------------------------------

# Aggregate data into Country/Region-wise (collapsing Province/Region), and remove Lat & Long
full_grouped <- full_table %>%
  group_by(Date, `Country/Region`) %>%
  summarise_at(c("Confirmed", "Deaths", "Recovered", "Active"), ~sum(.))

# Use 'cumulative' columns to get 'daily new' columns
full_grouped <- full_grouped %>%
  group_by(`Country/Region`) %>%
  mutate(
    NewConfirmed = Confirmed - lag(Confirmed),
    NewDeaths = Deaths - lag(Deaths),
    NewRecovered = Recovered - lag(Recovered)
  )
full_grouped = full_grouped %>% 
  mutate_at(c("NewConfirmed", "NewDeaths", "NewRecovered"), ~replace(., is.na(.), 0))
# replaces all NA entry (in first date) with 0

write.csv(full_grouped, "COVID-19-time-series-clean-complete.csv")







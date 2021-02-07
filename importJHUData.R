#' Imports and tidies global time series data for Covid-19 from JHU, exports
#' final data frame as csv file.

setwd("C:/users/mrome/Desktop/covid-19 data")

library(tidyverse)

confirmed_df <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_df <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_df <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

colnames(confirmed_df[-(1:4)]) # Gives all columns which are Dates

confirmed_df_long <- confirmed_df %>% pivot_longer(-(1:4), names_to = "Date", values_to = "count")
deaths_df_long <- deaths_df %>% pivot_longer(-(1:4), names_to = "Date", values_to = "count")
recovered_df_long <- recovered_df %>% pivot_longer(-(1:4), names_to = "Date", values_to = "count")

# Remove recovered date for Canada (as is counted Country-wise, whereas Confirmed & Deaths are counted Province-wise)
recovered_df_long <- recovered_df_long %>%
  mutate(count = ifelse(`Country/Region` == "Canada", NA, count))

# Merge (long) dataframes
full_table <- confirmed_df_long %>%
  left_join(deaths_df_long, by = c("Province/State", "Country/Region", "Lat", "Long", "Date")) %>%
  left_join(recovered_df_long, by = c("Province/State", "Country/Region", "Lat", "Long", "Date"))
colnames(full_table)[-(1:5)] <- c("Confirmed", "Deaths", "Recovered")

## Data Cleaning ------------------------------------------------------------------------------------
# Convert dates to type Date (from character)
full_table <- full_table %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) #, origin = as.Date("1970-01-01")

# Replace NA (in any column except Date) with 0
full_table <- full_table %>% mutate_at(setdiff(names(full_table), "Date"), ~replace(., is.na(.), 0))

# Extract and remove ship data from full_table
ship_rows <- full_table$`Province/State` %>% str_detect("Grand Princess") |
  full_table$`Province/State` %>% str_detect("Diamond Princess") |
  full_table$`Country/Region` %>% str_detect("Diamond Princess") |
  full_table$`Country/Region` %>% str_detect("MS Zaandam")

full_ship <- full_table[ship_rows,]
full_table <- full_table[!ship_rows,]

## Data Aggregation --------------------------------------------------------------------------------
full_table$Active <- full_table$Confirmed - full_table$Deaths - full_table$Recovered

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


  
  
  
  

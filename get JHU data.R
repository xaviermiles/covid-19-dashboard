#' Imports and tidies global time series data for Covid-19 from JHU, exports    
#' final data frame as csv file.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(magrittr)  # for two-way pipe


datasets <- c(
  "confirmed_global",
  "recovered_global",
  "deaths_global"
)
base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"

df_longs = vector(mode = "list")
for (dataset in datasets) {
  source_url <- paste0(base_url, dataset, ".csv")
  df <- read_csv(source_url, col_types = cols())
  
  df_long <- df %>%
    pivot_longer(-(1:4), names_to = "Date", values_to = "count") %>%
    mutate (Date = as.Date(Date, "%m/%d/%y"))
  
  if (dataset == "recovered_global") {
    df_long %>% filter(`Country/Region` == "Canada") %<>%  # NOTE two-way pipe
      mutate(`Province/State` = "All")
  }
  
  df_longs[[dataset]] <- df_long
}

#' Recovered data is counted for Canada as a whole, whereas Confirmed & Death 
#' data are counted for each Canadian province. So create count=NA rows for 
#' Confirmed & Deaths dataframes where {Country/Region=Canada, 
#' Province/State=All} before merging.
NA_rows <- df_longs[["confirmed_global"]] %>%
  filter(`Country/Region` == "Canada" & `Province/State` == "Alberta") %>%
  select(-count) %>%
  mutate(`Province/State` = "All", Lat = 	56.1304, Long = -106.3468, 
         Confirmed = NA, Deaths = NA)

# Merge three dataframes
global <- left_join(df_longs[["confirmed_global"]], df_longs[["deaths_global"]],
                    by = c("Province/State", "Country/Region", "Lat", "Long", 
                           "Date")) %>%
  rename(Confirmed = count.x, Deaths = count.y) %>%
  add_row(NA_rows) %>%
  left_join(., df_longs[["recovered_global"]],
            by = c("Province/State", "Country/Region", "Lat", "Long", "Date")) %>%
  rename(Recovered = count)

# Aggregate data into Country/Region-wise (collapsing Province/Region), and 
# remove Lat & Long. Also removes ship information.
ship_names <- c("Grand Princess", "Diamond Princess", "MS Zaandam")
global_country <- global %>%
  filter(!(`Province/State` %in% ship_names | 
             `Country/Region` %in% ship_names)) %>%
  group_by(`Country/Region`, Date) %>%
  summarise_at(c("Confirmed", "Deaths", "Recovered"), ~sum(., na.rm = TRUE))

# Use 'cumulative' columns to get 'daily new' columns
global_country <- global_country %>%
  group_by(`Country/Region`) %>%
  mutate(New_Confirmed = Confirmed - lag(Confirmed),
         New_Deaths = Deaths - lag(Deaths),
         New_Recovered = Recovered - lag(Recovered)) %>% 
  mutate_at(c("New_Confirmed", "New_Deaths", "New_Recovered"), 
            ~replace(., is.na(.), 0))

# Write to file
latest_date <- max(global_country$Date)
out_csv_filepath <- paste0("data/JHU_global_cases_", latest_date, ".csv")
write.csv(global_country, out_csv_filepath)
message(paste0("Writing JHU global cases to: ", out_csv_filepath))

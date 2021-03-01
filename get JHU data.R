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
    mutate(Date = as.Date(Date, "%m/%d/%y")) %>%
    rename(Country = `Country/Region`, State = `Province/State`)
  
  df_longs[[dataset]] <- df_long
}

#' Recovered data is counted for Canada as a whole, whereas Confirmed & Death 
#' data are counted for each Canadian province. So create count=NA rows for 
#' Confirmed & Deaths dataframes where {Country=Canada, State=All} before 
#' merging.
df_longs[["recovered_global"]] %>%
  mutate(State = if_else(Country == "Canada", "All", State))

NA_rows <- df_longs[["confirmed_global"]] %>%
  filter(Country == "Canada" & State == "Alberta") %>%
  select(-count) %>%
  mutate(State = "All", Lat = 	56.1304, Long = -106.3468, 
         Confirmed = NA, Deaths = NA)

# Merge three dataframes
global <- left_join(df_longs[["confirmed_global"]], df_longs[["deaths_global"]],
                    by = c("State", "Country", "Lat", "Long", "Date")) %>%
  rename(Confirmed = count.x, Deaths = count.y) %>%
  add_row(NA_rows) %>%
  left_join(., df_longs[["recovered_global"]],
            by = c("State", "Country", "Lat", "Long", "Date")) %>%
  rename(Recovered = count)

# Aggregate data into Country/Region-wise (collapsing Province/Region), and 
# remove Lat & Long. Also removes ship information.
ship_names <- c("Grand Princess", "Diamond Princess", "MS Zaandam")
global_country <- global %>%
  filter(!(State %in% ship_names | Country %in% ship_names)) %>%
  group_by(Country, Date) %>%
  summarise_at(c("Confirmed", "Deaths", "Recovered"), ~sum(., na.rm = TRUE))

# Use 'cumulative' columns to get 'daily new' columns
global_country <- global_country %>%
  group_by(Country) %>%
  mutate(Daily_Confirmed = c(0, diff(Confirmed)),
         Daily_Deaths = c(0, diff(Deaths)),
         Daily_Recovered = c(0, diff(Recovered)))

# Write to file
latest_date <- max(global_country$Date)
out_csv_filepath <- paste0("data/JHU_global_cases_", latest_date, ".csv")
write_csv(global_country, out_csv_filepath)
message(paste0("Writing JHU global cases to: ", out_csv_filepath))

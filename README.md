# Shiny dashboard for Covid-19 data

https://xavier-miles.shinyapps.io/covid-19_dashboard/

"app.R" runs "load data.R" to import the NZ Ministry of Health and John Hopkins University CSV datasets; then runs "ui.R" and "server.R" to construct the dashboard.

Original purpose was to re-create a histogram I saw displaying Covid-19 NZ data (but using R/ggplot2 rather than Tableau) and to learn how to use Shiny dashboards.
These original histograms are the DHB and Age stratified NZ tabs. The global tab was introduced to incoporate more data and to create a more interactive UI. 

**Future improvements:**
- Implement the individual country panels. This will include more detailed information/visuals for the country.
- Add 7-day average lineplot mode.
- Introduce some visualisations that involve maps.
- Add tables of data to main global panel.
- Make scaling of plots (eg. thousands, millions) more dynamic in the global tab, rather than if-else logic with hard-coded dependencies on the currently selected indicator.

Old/original version:
https://xavier-miles.shinyapps.io/covid-19_data/

# Shiny dashboard for Covid-19 data

https://xavier-miles.shinyapps.io/covid-19_data/

Main purpose was to re-create a histogram I saw displaying Covid-19 NZ data, but using R/ggplot2 rather than Tableau. Also wanted to learn how to make dashboards in R.
Was originally done in mid-2020. Currently uses a very old version of the Covid-19 data, as I had not added the functionality for it to automatically import the data itself.

"app.R" contains ALL of the logic for running the dashboard (this should be separated into modules at some point). 
"importJHUData.R" is a script to import international Covid-19 data from a github which contains information from John Hopkins University (probably needs to be re-configured), and this data was meant to be displayed included in the dashboard eventually.
"displaying data.R" and "module.R" are just testing scripts.

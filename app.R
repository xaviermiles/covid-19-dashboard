

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shinydashboard)
library(tidyverse)
library(readxl)
#source("module.R") #for if i decide to modularise into different files

#' to do:
#' > automatically importing latest NZ data 
#'      (a document which includes regional distinction ie. DHB column)
#' > change height of plot within ggplot/associated functions
#'     
#' > add functionality to Global sidebarMenu
#' 
#' > export to shareable format?? is this possible??



### Importing and tidying the data ###
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

ages_order = c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", 
               "50 to 59", "60 to 69", "70 to 79", "80 to 89", "90+")

nz_cases <- raw_nz_cases %>%
  rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
  mutate(report_date = as.Date(report_date, "%Y-%m-%d"),
         age_group = factor(age_group, levels = ages_order))




### Define UI ###
ui = dashboardPage(
  
  ## Header content
  dashboardHeader(
    title = "Welcome"
  ),
  
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Global", icon = icon("dashboard"), tabName = "global"
      ),
      menuItem(
        "New Zealand", icon = icon("kiwi-bird"), startExpanded = TRUE,
        menuSubItem("DHB Stratified", tabName = "DHBStratifiedTab", selected = TRUE),
        menuSubItem("Age Stratified", tabName = "ageStratifiedTab")
      )
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tabItems(
      tabItem("DHBStratifiedTab",
        fluidRow(
          box(
            status = "info", 
            width = 9,
            title="Cases by DHB",
            plotOutput("DHBStratifiedPlot")
          ),
          
          box(
            width = 2,
            actionButton("selectAllBtnDHB", "Select All"),
            actionButton("unselectAllBtnDHB", "Unselect All"),
            checkboxGroupInput("selectedDHBs", "Select DHB(s):",
                               choices = unique(nz_cases$dhb),
                               selected = unique(nz_cases$dhb)
            )
          )
        ),
        
        box(
          sliderInput("selectedDatesDHB", "Select Date Range:",
                      min = min(nz_cases$report_date), 
                      max = max(nz_cases$report_date),
                      value = as.Date(c("2020-02-26", "2020-05-20")))
        )
      ),
      
      tabItem("ageStratifiedTab",
        box(
          width = 9, status = "info",
          title = "Cases by Age",
          plotOutput("ageStratifiedPlot")
        ),
        box(
          width = 2,
          actionButton("selectAllBtnAge", "Select All"),
          actionButton("unselectAllBtnAge", "Unselect All"),
          checkboxGroupInput("selectedAges", "Select Age(s):",
                             choices = ages_order,
                             selected = ages_order)
        ),
        box(
          sliderInput("selectedDatesAge", "Select Date Range:",
                      min = min(nz_cases$report_date), 
                      max = max(nz_cases$report_date),
                      value = as.Date(c("2020-02-26", "2020-05-20")))
        )
      )
    )
  )
)



### Define server ###
server = function(input, output, session) {
  
  ## DHB Stratified Tab
  output$DHBStratifiedPlot = renderPlot({
    if (length(input$selectedDHBs) > 0) {
      nz_cases_subset <- nz_cases %>%
        subset(dhb %in% input$selectedDHBs) %>%
        count(report_date, dhb)
      alpha <- 1
    } else { # Display transparent image/representation of bar plot
      nz_cases_subset <- nz_cases %>%
        count(report_date, dhb)
      alpha <- 0.3
    }
    
    g <- ggplot(nz_cases_subset, 
                aes(report_date, n, fill = dhb)) +
      geom_bar(stat = "identity", alpha = alpha) +
      coord_cartesian(xlim = input$selectedDatesDHB,
                      ylim = c(0, 85)) +
      labs(x = "Report date", y = "Cases", fill = "DHBs",
           caption = "Source: Ministry of Health website")
    
    if (length(input$selectedDHBs) > 0) {
      g <- g + geom_text(aes(label = n), colour="white",
                         position = position_stack(vjust = 0.5))
    }
    
    return(g)
  })
  
  # Updates checkboxes if 'Select All' or 'Unselect All' button are pressed
  observeEvent(input$selectAllBtnDHB, {
    updateCheckboxGroupInput(
      session = session, inputId = "selectedDHBs",
      selected = unique(nz_cases$dhb)
    )
  })
  observeEvent(input$unselectAllBtnDHB, {
    updateCheckboxGroupInput(
      session, "selectedDHBs",
      selected = ""
    )
  })
  
  ## Age Stratified Tab
  output$ageStratifiedPlot = renderPlot({
    if (length(input$selectedAges) > 0) {
      nz_cases_subset <- nz_cases %>% 
        subset(age_group %in% input$selectedAges) %>%
        count(report_date, age_group)
      alpha <- 1
    } else {
      nz_cases_subset <- nz_cases %>%
        count(report_date, age_group)
      alpha <- 0.3
    }
    
    g <- ggplot(nz_cases_subset,
                aes(report_date, n, fill = age_group)) +
      geom_bar(stat = "identity", alpha = alpha) +
      coord_cartesian(xlim = input$selectedDatesAge,
                      ylim = c(0, 85)) +
      labs(x = "Report date", y = "Cases", fill = "Age groups",
           caption = "Source: Ministry of Health website")
    
    if (length(input$selectedAges) > 0) {
      g <- g + geom_text(aes(label = n), colour = "white",
                         position = position_stack(vjust = 0.5))
    }
    
    return(g)
  })
  
  # Updates checkboxes if 'Select All' or 'Unselect All' button are pressed
  observeEvent(input$selectAllBtnAge, {
    updateCheckboxGroupInput(
      session = session, inputId = "selectedAges",
      selected = unique(nz_cases$age_group)
    )
  })
  observeEvent(input$unselectAllBtnAge, {
    updateCheckboxGroupInput(
      session, "selectedAges",
      selected = ""
    )
  })
}



shinyApp(ui, server)
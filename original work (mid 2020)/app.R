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
latestNZData = "data/covid-19-case-list-17-april-2020.xlsx"
# is it possible to search for latest file?

# Import data (ignoring first 3 rows as are irrelevant):
confCases = read_excel(latestNZData, sheet = "Confirmed", skip = 3)
probCases = read_excel(latestNZData, sheet = "Probable", skip = 3)

# Changing column names:
newColNames = c("Date", "Sex", "Age", "DHB", "OverseasTravel", 
                "PreviousCountry", "FlightNumber", "DepartDate", "ArrivalDate")
colnames(confCases) = newColNames
colnames(probCases) = newColNames

# Changing some columns to 'Date' data type:
confCases = confCases %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
         DepartDate = as.Date(DepartDate),
         ArrivalDate = as.Date(ArrivalDate))
probCases = probCases %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
         DepartDate = as.Date(DepartDate),
         ArrivalDate = as.Date(ArrivalDate))

# Combine Confirmed and Probable cases into one object:
confCases$Status = rep("Confirmed", nrow(confCases))
probCases$Status = rep("Probable", nrow(probCases))
cases = rbind(confCases, probCases)

# Dictate: the order of stacking in the plots
cases$Status = factor(cases$Status, levels = c("Probable", "Confirmed"))
# d
agesOrder = c("<1", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 29",
              "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70+")
cases$Age = factor(cases$Age, levels=agesOrder)



### Define UI ###
ui = dashboardPage(
  
  ## Header content
  dashboardHeader(
    title = "Welcome everybody"
  ),
  
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Global", icon = icon("dashboard"), tabName = "global"
      ),
      menuItem(
        "New Zealand", icon = icon("kiwi-bird"), startExpanded = T,
        menuSubItem("Total", tabName = "totalCasesTab", selected = T),
        menuSubItem("Age Stratified", tabName = "ageStratifiedTab")
      )
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tabItems(
      tabItem("totalCasesTab",
        fluidRow(
          box(
            status = "info", 
            width = 9,
            title="Total Cases (Confirmed & Probable) - All Ages",
            plotOutput("totalCasesPlot")
          ),
          
          box(
            width = 2,
            actionButton("selectAllBtnTotal", "Select All"),
            actionButton("unselectAllBtnTotal", "Unselect All"),
            checkboxGroupInput("selectedDHBs", "Select DHB(s):",
              choices = unique(cases$DHB),
              selected = unique(cases$DHB)
            )
          )
        ),
        
        box(
          sliderInput("selectedDatesTotal", "Select Date Range:",
                      min = as.Date("2020-02-26"), 
                      max = as.Date("2020-04-17"),
                      value = c(as.Date("2020-02-26"), 
                                as.Date("2020-04-17")))
        )
      ),
      
      
      tabItem("ageStratifiedTab",
        box(
          width = 9, status = "info",
          title = "Cases by Age",
          plotOutput("ageStratifiedPlot")
        ),
        box(
          width=2,
          actionButton("selectAllBtnAge", "Select All"),
          actionButton("unselectAllBtnAge", "Unselect All"),
          checkboxGroupInput("selectedAges", "Select Age(s):",
                             choices = agesOrder,
                             selected = agesOrder)
        ),
        box(
          sliderInput("selectedDatesAge", "Select Date Range:",
                      min = as.Date("2020-02-26"), 
                      max = as.Date("2020-04-17"),
                      value=c(as.Date("2020-02-26"), 
                              as.Date("2020-04-17")))
        )
      )
    )
  )
)



### Define server ###
server = function(input, output, session) {
  
  ## Total Cases Tab
  output$totalCasesPlot = renderPlot({
    if (length(input$selectedDHBs) != 0) {
      ggplot(cases %>%
               #subset based on DHB checkboxes and Date slider
               subset((DHB %in% input$selectedDHBs) & 
                        (Date %in% seq(input$selectedDatesTotal[1], input$selectedDatesTotal[2], by="days"))) %>%
               #group by Date and Status, then count frequency for each
               count(Date, Status),
             
             aes(Date, n, fill = Status)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label=n), position=position_stack(vjust=0.5), colour="white") +
        coord_cartesian(xlim = c(input$selectedDatesTotal[1], input$selectedDatesTotal[2]),
                        ylim = c(0, 85)) +
        labs(y = "Cases", caption = "Source: Ministry of Health website")
    } else { # Display transparent image/representation of bar plot
      ggplot(cases %>%
               #subset based on Date slider
               subset(Date %in% seq(input$selectedDatesTotal[1], input$selectedDatesTotal[2], by="days")) %>%
               #group by Date and Status, then count frequency for each
               count(Date, Status),
             
             aes(Date, n, fill=Status)) +
        geom_bar(stat = "identity", alpha = 0.1) +
        coord_cartesian(xlim = c(input$selectedDatesTotal[1], input$selectedDatesTotal[2]), 
                        ylim = c(0 ,85)) +
        labs(y = "Cases", caption = "Source: Ministry of Health website")
    }
  })
  
  # Updates checkboxes if 'Select All' or 'Unselect All' button are pressed
  observeEvent(input$selectAllBtnTotal, {
    updateCheckboxGroupInput(
      session = session, inputId = "selectedDHBs",
      selected = unique(cases$DHB)
    )
  })
  observeEvent(input$unselectAllBtnTotal, {
    updateCheckboxGroupInput(
      session, "selectedDHBs",
      selected = ""
    )
  })
  
  
  ## Age Stratified Tab
  output$ageStratifiedPlot = renderPlot({
    if (length(input$selectedAges) != 0) { # If there are any Ages selected
      ggplot(cases %>% 
               #subselect based on Age checkboxes and Date slider
               subset((Age %in% input$selectedAges) & 
                        (Date %in% seq(input$selectedDatesAge[1], input$selectedDatesAge[2], by="days"))) %>%
               #group by Date and Age, then count frequency for each
               count(Date, Age),
             
             aes(Date, n, fill = Age)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=n), position=position_stack(vjust=0.5), colour="white") +
        coord_cartesian(xlim = c(input$selectedDatesAge[1], input$selectedDatesAge[2]),
                        ylim = c(0, 85)) +
        labs(y = "Cases", caption = "Source: Ministry of Health website")
    } else {
      ggplot(cases %>%
               #subselect based on Date slider
               subset(Date %in% seq(input$selectedDatesAge[1], input$selectedDatesAge[2], by="days")) %>%
               #group by Date and Age, then count frequency for each
               count(Date, Age),
             
             aes(Date, n, fill = Age)) +
        geom_bar(stat = "identity", alpha = 0.1) +
        coord_cartesian(xlim = c(input$selectedDatesAge[1], input$selectedDatesAge[2]), 
                        ylim = c(0, 85)) +
        labs(y = "Cases", caption = "Source: Ministry of Health website")
    }
  })
  
  # Updates checkboxes if 'Select All' or 'Unselect All' button are pressed
  observeEvent(input$selectAllBtnAge, {
    updateCheckboxGroupInput(
      session = session, inputId = "selectedAges",
      selected = unique(cases$Age)
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
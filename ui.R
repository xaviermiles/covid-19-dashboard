# Define the UI for the dashboard.

library(shinydashboard)


ui <- dashboardPage(
  
  dashboardHeader(
    title = "COVID-19",
    titleWidth = 200
  ),
  
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Global", icon = icon("dashboard"), tabName = "globalSuperTab"
      ),
      actionLink("remove", "Remove detail tabs"),
      menuItem(
        "New Zealand", icon = icon("kiwi-bird"), tabName = "NZ", 
        menuSubItem("DHB Stratified", tabName = "DHBStratifiedTab"),
        menuSubItem("Age Stratified", tabName = "ageStratifiedTab"),
        startExpanded = TRUE
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        "globalSuperTab",
        tabsetPanel(
          id = "globalTab",
          tabPanel(
            title = "Main dashboard",
            value = "page1",
            fluidRow(
              valueBoxOutput("total_confirmed"),
              valueBoxOutput("total_deaths"),
              column(sliderInput("num_top_countries", "Number of Countries:",
                                 min = 5, max = 15, value = 10, ticks = FALSE),
                     width = 2),
              column(uiOutput("open_panel_btn_spot"),
                     width = 2)
            ),
            fluidRow(
              valueBoxOutput("daily_confirmed", width = 2),
              valueBoxOutput("rolling_average_confirmed", width = 2),
              valueBoxOutput("daily_deaths", width = 2),
              valueBoxOutput("rolling_average_deaths", width = 2),
              column(selectInput("lineplot_mode", "Lineplot Mode:",
                                 choices = c("Daily", "Cumulative"),
                                 selected = "Cumulative"),
                     width = 2),
              column(selectInput("global_indicator", "Indicator:", 
                                 choices = c("Confirmed", "Deaths", 
                                             "Infection Rate")),
                     width = 2)
            ),
            fluidRow(),
            fluidRow(
              column(
                width = 6,
                plotOutput("global_plot")
              ),
              column(
                width = 6,
                plotOutput("top_countries_plot", 
                           click = "top_countries_plot_click")
              )
            ),
            dateRangeInput("global_dates", "Dates:",
                           start = min(global_cases$Date),
                           end = max(global_cases$Date),
                           min = min(global_cases$Date),
                           max = max(global_cases$Date),
                           format = "dd-mm-yyyy",
                           width = "20%"),
            fluidRow(
              tags$table(
                tags$tr(width = "15%",
                        tags$td(width = "40%", "Last n months:"),
                        tags$td(width = "60%", textInput("global_dates_preset", label = NULL, width = "80%")))
              ),
              actionButton("global_dates_reset", "Reset"),
            )
            #, tags$head(tags$style(".small-box{height:75px}"))
          )
        )
      ),
      
      tabItem(
        "DHBStratifiedTab",
        column(width = 9,
               box(
                 width = 12, height = 500, status = "info",
                 title = "Cases by DHB",
                 plotOutput("DHBStratifiedPlot")
               ),
               box(
                 width = 6, status = "info",
                 sliderInput("selectedDatesDHB", "Select Date Range:",
                             min = min(nz_cases$report_date), 
                             max = max(nz_cases$report_date),
                             value = as.Date(c("2020-02-26", "2020-05-20")))
               )
        ),
        
        box(
          width = 3,
          actionButton("selectAllBtnDHB", "Select All"),
          actionButton("unselectAllBtnDHB", "Unselect All"),
          checkboxGroupInput("selectedDHBs", "Select DHB(s):",
                             choices = unique(nz_cases$dhb),
                             selected = unique(nz_cases$dhb))
        )
      ),
      
      tabItem(
        "ageStratifiedTab",
        column(width = 9,
               box(
                 width = 12, height = 500, status = "info",
                 title = "Cases by Age",
                 plotOutput("ageStratifiedPlot")
               ),
               box(
                 width = 6, status = "info",
                 sliderInput("selectedDatesAge", "Select Date Range:",
                             min = min(nz_cases$report_date),
                             max = max(nz_cases$report_date),
                             value = as.Date(c("2020-02-26", "2020-05-20")))
               )
        ),
        box(
          width = 3,
          actionButton("selectAllBtnAge", "Select All"),
          actionButton("unselectAllBtnAge", "Unselect All"),
          checkboxGroupInput("selectedAges", "Select Age Group(s):",
                             choices = ages_order,
                             selected = ages_order)
        )
      )
    )
  )
)
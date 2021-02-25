

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shinydashboard)
library(tidyverse)
library(magrittr)
library(r2d3)
#source("module.R") #for if i decide to modularise into different files

#' to do:
#' > automatically importing latest NZ data 
#'      (a document which includes regional distinction ie. DHB column)
#' > change height of plot within ggplot/associated functions
#'     
#' > add functionality to Global sidebarMenu
#' 
#' > export to shareable format?? is this possible??


### Nice theme for ggplots ###
custom_theme <- theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray"),
  panel.grid.minor.y = element_line(color = "gray"),
  axis.ticks = element_blank(),
  panel.background = element_blank(),
  legend.key = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5)
)


### Importing and tidying the NZ data ###
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


### Importing and tidying global data ###
jhu_filenames <- list.files("./data", 
                            pattern = "JHU_global_cases\\d{4}-\\d{2}-\\d{2}.csv") %>%
  sort(decreasing = TRUE)

global_cases <- read_csv("data/JHU_global_cases_2021-02-17.csv",
                         col_types = cols()) %>%
  mutate(`Country/Region` = factor(`Country/Region`))



### Define UI ###
ui = dashboardPage(

  dashboardHeader(
    title = "COVID-19",
    titleWidth = 200
  ),
  
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Global", icon = icon("dashboard"), tabName = "globalSuperTab",
        selected = TRUE
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
  
  
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(
        "globalSuperTab",
        tabsetPanel(
          id = "globalTab",
          tabPanel(
            title = "Main dashboard",
            value = "page1",
            fluidRow(class = "summaryRow",
              valueBoxOutput("total_confirmed"),
              valueBoxOutput("total_deaths"),
              column(sliderInput("num_top_countries", "Number of Countries:",
                                 min = 5, max = 15, value = 10, ticks = FALSE),
                     width = 2),
              column(uiOutput("open_panel_btn_spot"),
                     width = 2)
            ),
            fluidRow(class = "summaryRow",
              valueBoxOutput("new_confirmed", width = 2),
              valueBoxOutput("rolling_average_confirmed", width = 2),
              valueBoxOutput("new_deaths", width = 2),
              valueBoxOutput("rolling_average_deaths", width = 2),
              column(selectInput("lineplot_mode", "Lineplot Mode:",
                                 choices = c("Daily", "Cumulative"),
                                 selected = "Cumulative"),
                     width = 2),
              column(selectInput("global_mode", "Mode:",
                                 choices = c("Confirmed", "Deaths")),
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
            tags$head(tags$style(".small-box{height:75px}"))
          )
        )
      ),
      
      tabItem(
        "DHBStratifiedTab",
        box(
          status = "info", 
          width = 9,
          title = "Cases by DHB",
          plotOutput("DHBStratifiedPlot")
        ),
        
        box(
          width = 2,
          actionButton("selectAllBtnDHB", "Select All"),
          actionButton("unselectAllBtnDHB", "Unselect All"),
          checkboxGroupInput("selectedDHBs", "Select DHB(s):",
                             choices = unique(nz_cases$dhb),
                             selected = unique(nz_cases$dhb))
        ),
        
        box(
          sliderInput("selectedDatesDHB", "Select Date Range:",
                      min = min(nz_cases$report_date), 
                      max = max(nz_cases$report_date),
                      value = as.Date(c("2020-02-26", "2020-05-20")))
        )
      ),
      
      tabItem(
        "ageStratifiedTab",
        fluidRow(
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
          )
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
  
  # Global: Panel Open/Close  --------------------------------------------------
  panel_list <- NULL
  
  reac <- reactiveValues(toHighlight = rep(FALSE, 10),
                         selectedBar = NULL)
  
  observeEvent(input$top_countries_plot_click, {
    top_countries <- global_cases %>%
      filter(Date == max(global_cases$Date)) %>%
      slice_max(order_by = .data[[input$global_mode]], 
                n = input$num_top_countries)
    
    new_selected_bar <- top_countries %>%
      .$`Country/Region` %>%
      reorder(top_countries[[input$global_mode]]) %>%
      .[round(input$top_countries_plot_click$x)]
    
    if (!is.null(reac$selectedBar) && reac$selectedBar == new_selected_bar) {
      # deselect bar
      reac$selectedBar <- NULL
      reac$toHighlight <- rep(FALSE, input$num_top_countries)
      output$open_panel_btn_spot <- renderUI(NULL)
    } else {
      # select new bar
      reac$selectedBar <- new_selected_bar
      reac$toHighlight <- top_countries$`Country/Region` %in% reac$selectedBar
      output$open_panel_btn_spot <- renderUI(
        actionButton("open_panel_btn", "View more details")
      )
    }
  })
  
  # open new panel
  observeEvent(input$open_panel_btn, {
    panel_title <- as.vector(reac$selectedBar)
    
    if (!(panel_title %in% panel_list)) {
      appendTab(
        inputId = "globalTab",
        tabPanel(
          panel_title,
          valueBox(panel_title, "hi")
        )
      )
      panel_list <<- c(panel_list, panel_title)
    }
    updateTabsetPanel(session, "globalTab", selected = panel_title)
  })
  
  # close all panels
  observeEvent(input$remove, {
    panel_list %>%
      walk(~ removeTab("globalTab", .x))
    
    panel_list <<- NULL
  })
  
  # Global: Number of Countries Slider -----------------------------------------
  observeEvent(input$num_top_countries, {
    if (input$num_top_countries <= length(reac$toHighlight)) {
      reac$toHighlight <- reac$toHighlight[1:input$num_top_countries]
    } else {
      diff <- input$num_top_countries - length(reac$toHighlight)
      reac$toHighlight <- c(reac$toHighlight, rep(FALSE, diff))
    }
  })
  
  # change the scaling of the plots according the selected mode
  scaling_factor <- reactiveValues(num = 1e6, text = "millions")
  
  observeEvent(input$global_mode, {
    if (input$global_mode == "Confirmed") {
      scaling_factor$num <- 1e6
      scaling_factor$text <- "millions"
    } else if (input$global_mode == "Deaths") {
      scaling_factor$num <- 1e3
      scaling_factor$text <- "thousands"
    } else {
      warning("Mode does not have scaling factor for plots.")
    }
  })
  
  # Global: Summary boxes ------------------------------------------------------
  output$total_confirmed <- renderValueBox({
    text <- global_cases %>%
      filter(Date == max(unique(global_cases$Date))) %>%
      .$Confirmed %>%
      sum() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "Total Confirmed")
  })
  
  output$total_deaths <- renderValueBox({
    global_cases %>%
      filter(Date == max(unique(global_cases$Date))) %>%
      .$Deaths %>%
      sum() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "Total Deaths")
  })
  
  ## Global: Sub-summary Boxes -------------------------------------------------
  output$new_confirmed <- renderValueBox({
    global_cases %>%
      filter(Date == max(unique(global_cases$Date))) %>%
      .$New_Confirmed %>%
      sum() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "Daily Confirmed")
  })
  
  output$rolling_average_confirmed <- renderValueBox({
    last_date <- max(global_cases$Date)
    rolling_dates <- seq.Date(last_date - 6, last_date, by = "days")
    
    global_cases %>%
      filter(Date %in% rolling_dates) %>%
      .$New_Confirmed %>%
      sum() %>%
      divide_by(7) %>%
      round() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "7-day Average Confirmed")
  })
  
  output$new_deaths <- renderValueBox({
    global_cases %>%
      filter(Date == max(unique(global_cases$Date))) %>%
      .$New_Deaths %>%
      sum() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "Daily Deaths")
  })
  
  output$rolling_average_deaths <- renderValueBox({
    last_date <- max(global_cases$Date)
    rolling_dates <- seq.Date(last_date - 6, last_date, by = "days")
    
    global_cases %>%
      filter(Date %in% rolling_dates) %>%
      .$New_Deaths %>%
      sum() %>%
      divide_by(7) %>%
      round() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "7-day Average Deaths")
  })
  
  # Global: Plots --------------------------------------------------------------
  output$global_plot = renderPlot({
    top_countries <- global_cases %>%
      filter(Date == max(global_cases$Date)) %>%
      slice_max(order_by = .data[[input$global_mode]], n = input$num_top_countries) %>%
      .$`Country/Region`

    global_subset <- global_cases %>%
      filter(`Country/Region` %in% top_countries)
    
    if (input$lineplot_mode == "Daily") {
      full_mode <- paste0("New_", input$global_mode)
    } else {
      full_mode <- input$global_mode
    }
    print(as.name(full_mode))
    
    ggplot(global_subset, aes(Date, .data[[full_mode]], 
                              group = `Country/Region`)) +
      geom_line(aes(color = `Country/Region`), size = 1) +
      labs(x = "", y = "", title = paste0("Cumulative ", input$global_mode, 
                                          " (", scaling_factor$text, ")")) +
      scale_x_date(date_labels = "%b-%y", breaks = "1 month") +
      scale_y_continuous(labels = function(x) {x / scaling_factor$num}) +
      custom_theme
  })
  
  output$top_countries_plot = renderPlot({
    top_countries_total <- global_cases %>%
      filter(Date == max(global_cases$Date)) %>%
      slice_max(order_by = .data[[input$global_mode]], 
                n = input$num_top_countries)
    
    ggplot(top_countries_total, 
           aes(x = reorder(`Country/Region`, -.data[[input$global_mode]]), 
               y = .data[[input$global_mode]],
               fill = ifelse(reac$toHighlight, "yes", "no"))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("yes" = "red", "no" = "lightblue" ), 
                        guide = FALSE) +
      labs(x = "", y = "",  title = paste0("Total ", input$global_mode,
                                           " (", scaling_factor$text, ")")) +
      scale_y_continuous(labels = function(x) {x / scaling_factor$num}) +
      custom_theme
  })
  
  # NZ: DHB Stratified Tab -----------------------------------------------------
  output$DHBStratifiedPlot <- renderPlot({
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
  
  # NZ: Age Stratified Tab -----------------------------------------------------
  output$ageStratifiedPlot <- renderPlot({
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
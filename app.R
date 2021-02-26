

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
  legend.position = "left",
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
  mutate(Country = factor(Country), Date = as.Date(Date, "%d/%m/%Y")) %>%
  mutate(Infection_Rate = Deaths / Confirmed,
         Daily_Infection_Rate = Daily_Deaths / Daily_Confirmed)

global_cases <- global_cases %>%
  mutate(Infection_Rate = if_else(is.infinite(Infection_Rate), 1, Infection_Rate))


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
                         selectedBar = NULL,
                         selectedColor = NULL)
  
  # a bar is clicked
  observeEvent(input$top_countries_plot_click, {
    indicator <- gsub(" ", "_", input$global_indicator)
    
    top_countries <- global_cases %>%
      filter(Date == max(global_cases$Date)) %>%
      slice_max(order_by = .data[[indicator]], 
                n = input$num_top_countries)
    
    new_selected_bar <- top_countries %>%
      .$Country %>%
      reorder(top_countries[[indicator]]) %>%
      .[round(input$top_countries_plot_click$x)]
    
    if (!is.null(reac$selectedBar) && reac$selectedBar == new_selected_bar) {
      # deselect bar
      reac$selectedBar <- NULL
      reac$toHighlight <- rep(FALSE, input$num_top_countries)
      reac$selectedColor <- NULL
      
      output$open_panel_btn_spot <- renderUI(NULL)
    } else {
      # select new bar
      reac$selectedBar <- new_selected_bar
      reac$toHighlight <- top_countries$Country %in% reac$selectedBar
      print(top_countries)
      print(new_selected_bar)
      print(reac$toHighlight)
      reac$selectedColor <- scales::hue_pal()(input$num_top_countries)[which(reac$toHighlight)]
      
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
  
  # Global: Scaling of Axis Labels ---------------------------------------------
  scaling_factor <- reactiveValues(num = 1e6, text = "millions")
  
  observeEvent(input$global_indicator, {
    
    if (input$global_indicator == "Confirmed") {
      scaling_factor$num <- 1e6
      scaling_factor$text <- " (millions)"
    } else if (input$global_indicator == "Deaths") {
      scaling_factor$num <- 1e3
      scaling_factor$text <- " (thousands)"
    } else if (input$global_indicator == "Infection Rate") {
      scaling_factor$num <- 1
      scaling_factor$text <- ""
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
  
  ## Global: Sub-Summary Boxes -------------------------------------------------
  output$daily_confirmed <- renderValueBox({
    global_cases %>%
      filter(Date == max(unique(global_cases$Date))) %>%
      .$Daily_Confirmed %>%
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
      .$Daily_Confirmed %>%
      sum() %>%
      divide_by(7) %>%
      round() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "7-day Average Confirmed")
  })
  
  output$daily_deaths <- renderValueBox({
    global_cases %>%
      filter(Date == max(unique(global_cases$Date))) %>%
      .$Daily_Deaths %>%
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
      .$Daily_Deaths %>%
      sum() %>%
      divide_by(7) %>%
      round() %>%
      prettyNum(big.mark = ",") %>%
      tags$p(style = "font-size: 80%;") %>%
      valueBox(subtitle = "7-day Average Deaths")
  })
  
  # Global: Plots --------------------------------------------------------------
  output$global_plot = renderPlot({
    indicator <- gsub(" ", "_", input$global_indicator)
    
    top_countries <- global_cases %>%
      filter(Date == max(global_cases$Date)) %>%
      slice_max(order_by = .data[[indicator]], 
                n = input$num_top_countries) %>%
      .$Country

    global_subset <- global_cases %>%
      filter(Country %in% top_countries) %>%
      na.omit()
    
    if (input$lineplot_mode == "Daily") {
      y_var_name <- paste0("Daily_", indicator)
    } else {
      y_var_name <- indicator
    }
    
    suppressWarnings(suppressMessages(
    ggplot(global_subset, aes(Date, .data[[y_var_name]], group = Country)) +
      geom_line(color = reac$selectedColor, size = 2.5) +
      labs(x = "", y = "", title = paste0("Cumulative ", input$global_indicator, 
                                          scaling_factor$text)) +
      scale_x_date(date_labels = "%b-%y", breaks = "1 month") +
      scale_y_continuous(labels = function(x) {x / scaling_factor$num}) +
      gghighlight::gghighlight(Country == reac$selectedBar,
                               unhighlighted_params = list(size = 2),
                               keep_scales = TRUE) +
      custom_theme
    ))
  })
  
  output$top_countries_plot = renderPlot({
    indicator <- gsub(" ", "_", input$global_indicator)
    
    top_countries_total <- global_cases %>%
      filter(Date == max(global_cases$Date)) %>%
      slice_max(order_by = .data[[indicator]], 
                n = input$num_top_countries)
    
    y_var_name <- indicator
    
    ggplot(top_countries_total, 
           aes(x = reorder(Country, -.data[[y_var_name]]), 
               y = .data[[y_var_name]],
               fill = if_else(reac$toHighlight, "yes", "no"))) +
      geom_col() +
      geom_text(aes(label = round(.data[[y_var_name]] / scaling_factor$num, 2)), 
                vjust = -0.5) +
      scale_fill_manual(values = c("yes" = reac$selectedColor, 
                                   "no" = "lightblue"), 
                        guide = FALSE) +
      labs(x = "", y = "",  title = paste0("Total ", input$global_indicator,
                                           scaling_factor$text)) +
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
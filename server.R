#' Define the server logic for the dashboard.

library(tidyverse)
library(magrittr)
library(lubridate)


# Nice theme for ggplots
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

server <- function(input, output, session) {
  
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
  
  observeEvent(input$global_dates_reset, {
    updateDateRangeInput(session, "global_dates",
                         start = min(global_cases$Date),
                         end = max(global_cases$Date))
    updateTextInput(session, "global_dates_preset", value = "")
  })
  
  observeEvent(input$global_dates_preset, {
    tryCatch({
      int_entry <- as.integer(input$global_dates_preset)
      
      if (!is.na(int_entry) && int_entry <= 0) {
        # Integer is negative, so skip to 'Catch' code
        warning()
      }
      
      end <- max(global_cases$Date)
      start <- max(end %m-% months(int_entry), min(global_cases$Date))
      
      updateDateRangeInput(session, "global_dates",
                           start = start,
                           end = end)
      # Rounds up in case the entire date range was selected
      updateTextInput(session, "global_dates_preset", 
                      value = ceiling(interval(start, end) / months(1)))
    },
    warning=function(cond) {
      updateTextInput(session, "global_dates_preset", value = "")
    })
  })
  
  # Global: Scaling of Axis Labels ---------------------------------------------
  scaling_factor <- reactiveValues(num = 1e6, text = "millions")
  
  observeEvent(input$global_indicator, {
    # Remove selection
    reac$toHighlight <- rep(FALSE, 10)
    reac$selectedBar <- NULL
    reac$selectedColor <- NULL
    
    # Change the scaling of the plots
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
    
    # Change the selection of lineplot_mode, if necessary
    if (input$global_indicator == "Infection Rate") {
      new_selection <- c("Cumulative")
    } else {
      new_selection <- c("Daily", "Cumulative")
    }
    new_selected <- if_else(input$lineplot_mode %in% new_selection,
                            input$lineplot_mode,
                            new_selection[1])
    updateSelectInput(session, "lineplot_mode",
                      choices = new_selection,
                      selected = new_selected)
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
    
    # Subset by relevant countries and selected date range
    global_subset <- global_cases %>%
      filter(input$global_dates[1] < Date,
             Date < input$global_dates[2],
             Country %in% top_countries) %>%
      na.omit()
    
    if (input$lineplot_mode == "Daily") {
      y_var_name <- paste0("Daily_", indicator)
    } else {
      y_var_name <- indicator
    }
    
    # Prevent plot loading if the y_var_name is not configured correctly
    req(y_var_name %in% colnames(global_subset))
    
    suppressWarnings(suppressMessages(
      ggplot(global_subset, aes(Date, .data[[y_var_name]], group = Country)) +
        geom_line(color = reac$selectedColor, size = 2.5) +
        labs(x = "", y = "", title = paste0(input$lineplot_mode, " ",
                                            input$global_indicator, 
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
    
    ggplot(top_countries_total, 
           aes(x = reorder(Country, -.data[[indicator]]), 
               y = .data[[indicator]],
               fill = if_else(reac$toHighlight, "yes", "no"))) +
      geom_col() +
      geom_text(aes(label = round(.data[[indicator]] / scaling_factor$num, 2)), 
                vjust = -0.5) +
      scale_fill_manual(values = c("yes" = reac$selectedColor, 
                                   "no" = "lightblue"), 
                        guide = FALSE) +
      labs(x = "", y = "",  
           title = paste0("Total ", input$global_indicator, scaling_factor$text),
           caption = "Try clicking on a bar") +
      scale_y_continuous(labels = function(x) {x / scaling_factor$num}) +
      custom_theme
  }, height = 420)
  
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
      labs(x = "Report Date", y = "Cases", fill = "DHB(s)",
           caption = "Source: Ministry of Health Website") + 
      guides(fill = guide_legend(ncol = 1))
    
    if (length(input$selectedDHBs) > 0) {
      g <- g + geom_text(aes(label = n), colour="white",
                         position = position_stack(vjust = 0.5))
    }
    
    return(g)
  }, height = 420)
  
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
      labs(x = "Report Date", y = "Cases", fill = "Age Group(s)",
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
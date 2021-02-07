library("readxl")
library("padr")

totalCasesModuleUI = function(id) {
  ns = NS(id)
  
  tagList(
    plotOutput(ns("totalCasesPlot")),
    checkboxGroupInput(ns("DHBs"), "Select DHB(s)", choices=unique(cases$DHB))
  )
}

totalCasesModule = function(input, output, session, data) {
  latestFigures = "covid-19-case-list-17-april-2020.xlsx"
  # is it possible to search for latest file?
  
  confCases = read_excel(latestFigures, sheet="Confirmed", range="A4:I9999")
  confCases = confCases[rowSums(is.na(confCases)) != ncol(confCases),]
  
  probCases = read_excel(latestFigures, sheet="Probable", range="A4:I9999")
  probCases = probCases[rowSums(is.na(probCases)) != ncol(probCases),]
  # want to ignore first 3 rows
  # at the moment imports an arbitrarily large number of rows, then removes all empty rows
  # would prefer to not import empty rows in the first place -> is that possible??
  
  
  # Changing column names:
  newColNames = c("Date", "Sex", "Age", "DHB", "OverseasTravel", "PreviousCountry", "FlightNumber", 
                  "DepartDate", "ArrivalDate")
  colnames(confCases) = newColNames
  colnames(probCases) = newColNames
  
  
  # Changing some columns to 'Date' data type:
  confCases = confCases %>% mutate(Date=as.Date(Date, format="%d/%m/%Y"),
                                   DepartDate=as.Date(DepartDate),
                                   ArrivalDate=as.Date(ArrivalDate))
  probCases = probCases %>% mutate(Date=as.Date(Date, format="%d/%m/%Y"),
                                   DepartDate=as.Date(DepartDate),
                                   ArrivalDate=as.Date(ArrivalDate))
  
  
  # Combine Confirmed and Probable cases into one object:
  confCases$Status = rep("Confirmed", nrow(confCases))
  probCases$Status = rep("Probable", nrow(probCases))
  cases = rbind(confCases, probCases)
  #change the order of stacking in the plot
  cases$Status = factor(cases$Status, levels=c("Probable", "Confirmed")) 

  # Setting up plot:
  ggplot(cases %>% count(Date, Status), #group by date and status, then count number
         aes(Date, n, fill=Status)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=n), position=position_stack(vjust=0.5), colour="white") +
    labs(title="Total Cases (Confirmed & Probable) - All Ages",
         y="Cases",
         caption="Source: Ministry of Health website")
}
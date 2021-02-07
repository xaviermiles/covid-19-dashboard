setwd("C:/users/mrome/Desktop/covid-19 data")

library("tidyverse")
library("readxl")
library("plotly")

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
cases$Status = factor(cases$Status, levels=c("Probable", "Confirmed")) 
#this dictates the order of stacking in the plot


# Getting number of cases per given day:
library("padr")

confCases_tally = count(confCases, vars=Date)
colnames(confCases_tally) = c("Date", "Freq")
confCases_tally = confCases_tally %>% pad("day") 
#adds row for missing days, with NA entries for Freq
confCases_tally[is.na(confCases_tally)] = 0 #changes NA entries to zero

#do again with probable cases
probCases_tally = count(probCases, vars=Date)
colnames(probCases_tally) = c("Date", "Freq")
probCases_tally = probCases_tally %>% pad("day", start_val=confCases_tally$Date[1])
#adds row for missing days, with NA entries for Freq
probCases_tally[is.na(probCases_tally)] = 0 #changes NA entries to zero

#combine comfirmed and probable tallies
confCases_tally$Status = rep("Confirmed", nrow(confCases_tally))
probCases_tally$Status = rep("Probable", nrow(probCases_tally))
cases_tally = rbind(confCases_tally, probCases_tally)

#simple tally that doesnt distinguish cases by Status
cases_simple_tally = count(cases, vars=Date)
colnames(cases_simple_tally) = c("Date", "Freq")
cases_simple_tally = cases_simple_tally %>% pad("day")
cases_simple_tally[is.na(cases_simple_tally)] = 0





# Setting up plot:

ggplot(cases_tally, aes(x=Date, y=Freq, fill=Status)) + 
  geom_bar(position="identity") +
  geom_text(aes(label="Gherkin"), position=position_stack(vjust=0.5))

pal = cbind(cases_simple_tally$Date, cases_simple_tally$Freq<6)

plot1 = ggplot(cases %>% count(Date, Status), #group by date and status, then count number
       aes(Date, n, fill=Status)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n), position=position_stack(vjust=0.5), colour="white") +
  labs(title="Total Cases (Confirmed & Probable) - All Ages",
       y="Cases",
       caption="Source: Ministry of Health website")



#using plotly:
# fig1 = ggplotly(plot1)
# fig1

  








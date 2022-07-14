library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
###IMPORTANT, Each indicator should have a column name specific to itself###


##Raw Data File
LIHTC <- read_csv("Data\\RawData\\HUD\\LIHTC\\LIHTC_AssistedUnits.csv", col_names  = TRUE)

##Tidycensus Call to get 2020 Census Data for Total Households by County for State of Iowa
House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)

##Filters for only Data in State of Iowa
##Aggregates total number of Low Income units for every project by County
LIHTCSum <- LIHTC %>%
  filter(STD_ST == "IA")%>%
  group_by(COUNTY_LEVEL)%>%
  summarise(LI = sum(LI_UNITS, na.rm =TRUE))


##Calculates the Number of Units that will have Initial Compliance Expiring for each given year (15 Years from Date Placed in Service)
##Calculates the Total Number of Units For each County
##Filters to only account for the next 5 years based on Sys.Date() and calculates the percentage of units in those 5 years divided by the Total number of units
LIHTCSum2 <- LIHTC %>%
  filter(STD_ST == "IA")%>%
  mutate(Year= YR_PIS)%>%
  group_by(COUNTY_LEVEL,Year)%>%
  summarise(LI = sum(LI_UNITS, na.rm=TRUE))%>%
  ungroup%>%
  group_by(COUNTY_LEVEL)%>%
  mutate(TotalLI = sum(LI))%>%
  ungroup%>%
  mutate(YearEnd = ifelse(Year==8888,Year,Year+15))%>%
  filter(!is.na(COUNTY_LEVEL))%>%
  group_by(COUNTY_LEVEL)%>%
  filter(YearEnd<= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")) +5 & YearEnd>= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")))%>%
  mutate(LILost5Year = sum(LI))%>%
  mutate(PercentLILost5Year =  LILost5Year/TotalLI *100)%>%
  select(COUNTY_LEVEL,PercentLILost5Year)%>%  ##Only Care About Final Percentage Lost of 5 Year
  distinct() ##Used To Remove Duplicate Rows

##Joining the 2020 Census Data with Data of Total LIHTC Units by County 
##Turns it into a Percent of Units by Total Households
Overall <- merge(House2020,LIHTCSum, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE) %>%
  mutate(PercentLIHTCperHousehold = LI/value*100)

##Joins Previous Data with Data of Percent Loss over 5 Years
##Sets Percent Loss over 5 years to Zero if it has no Value (No Projects Ending in that county in 5 years)
Overall <- merge(Overall,LIHTCSum2, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE)%>%
  mutate(PercentLILost5Year  = ifelse(is.na(PercentLILost5Year),0,PercentLILost5Year))

##Only Wants to have Fips Code, and 2 indicator Values
##This should be the cleaned final form
Output <- Overall %>%
  select(FipsCode =  GEOID,PercentLIHTCperHousehold,PercentLILost5Year)

##Saves Clean Data to File
##row.names NEEDS to be false
##otherwise First Column will not be Fips Code
write.csv(Output, "Data\\CleanData\\Indicator_LIHTC_AssistedUnits.csv", row.names = FALSE)


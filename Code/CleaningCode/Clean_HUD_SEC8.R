library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
###IMPORTANT, Each indicator should have a column name specific to itself###


##Raw Data File
HUD <- read_csv("Data\\RawData\\HUD\\Raw_multiFamAssistUnits.csv", col_names  = TRUE)

##Tidycensus Call to get 2020 Census Data for Total Households by County for State of Iowa
House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)


##Filters for only Data in State of Iowa
MultiData <- HUD %>%
  filter(STD_ST == "IA")

##Aggregates total number of units for every project by County
MultiDataSum <- MultiData %>%
  group_by(COUNTY_LEVEL)%>%
  summarise(TotalAssisted = sum(TOTAL_ASSISTED_UNIT_COUNT,na.rm =TRUE))

##Calculates the Number of Units that will have Contract Expiring for each given year
##Calculates the Total Number of Units For each County
##Filters to only account for the next 5 years based on Sys.Date() and calculates the percentage of units in those 5 years divided by the Total number of units
HudMultiData<- MultiData %>%
  mutate(Year = 2000+ as.numeric(substr(EXPIRATION_DATE1,nchar(EXPIRATION_DATE1)-1,nchar(EXPIRATION_DATE1))))%>%
  group_by(COUNTY_LEVEL,Year)%>%
  summarise(Units = sum(TOTAL_ASSISTED_UNIT_COUNT, na.rm=TRUE))%>%
  ungroup%>%
  group_by(COUNTY_LEVEL)%>%
  mutate(TotalUnits = sum(Units))%>%
  filter(Year<= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")) +5 & Year>= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")))%>%
  mutate(HudSec8Lost5Year = sum(Units))%>%
  mutate(PercentHudSec8Lost5Year = HudSec8Lost5Year/TotalUnits *100)%>%
  select(COUNTY_LEVEL,PercentHudSec8Lost5Year)%>% ##Only Care About Final Percentage Lost of 5 Year
  distinct()##Used To Remove Duplicate Rows

##Joining the 2020 Census Data with Data of Total Units by County 
##Turns it into a Percent of Units by Total Households
Overall <- merge(House2020,MultiDataSum, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE) %>%
  mutate(PercentMultiFamAssistPerHouseold = TotalAssisted/value *100)

##Joins Previous Data with Data of Percent Loss over 5 Years
##Sets Percent Loss over 5 years to Zero if it has no Value (No Projects Ending in that county in 5 years)
Overall <- merge(Overall,HudMultiData, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE)%>%
  mutate(PercentHudSec8Lost5Year  = ifelse(is.na(PercentHudSec8Lost5Year),0,PercentHudSec8Lost5Year))

##Only Wants to have Fips Code, and indicator Values
##This should be the cleaned final form
Output <- Overall %>%
  select(FipsCode=GEOID,PercentMultiFamAssistPerHouseold,PercentHudSec8Lost5Year)

##Saves Clean Data to File
##row.names NEEDS to be false
##otherwise First Column will not be Fips Code
write.csv(Output, "Data\\CleanData\\Ready_HUD_multiFamAssistUnits.csv", row.names = FALSE)

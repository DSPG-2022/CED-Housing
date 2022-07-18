library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
###IMPORTANT, Each indicator should have a column name specific to itself


##Raw Data File
USDAData <- read_csv("Data\\RawData\\USDA\\Raw_USDA_Section515.csv", col_names  = TRUE)

##Filters for only Data in State of Iowa
USDAData <-USDAData %>%
  filter(substr(State_County_FIPS_Code,0,2)==19)

##Aggregates total number of units for every project by County
USDASum <- USDAData%>%
  group_by(State_County_FIPS_Code)%>%
  summarise(TotalUnits = sum(Project_Size))

##Tidycensus Call to get 2020 Census Data for Total Households by County for State of Iowa
source("Code\\GatheringCode\\Get_CensusDecenial_TotalHousingUnits.R")


##Creates a Column for Year based on the year given in the Date_Restrictive_Clause_Expires
##If No Year is given, assign it to 8888 to keep consistent with HUD LIHTC year naming scheme
USDADataSum <-USDAData %>%
  mutate(Year=ifelse(is.na(Date_Restrictive_Clause_Expires),8888,substr(Date_Restrictive_Clause_Expires,nchar(Date_Restrictive_Clause_Expires)-3,nchar(Date_Restrictive_Clause_Expires))))


##Calculates the Number of Units that will have Restrictive Clause Expiring for each given year
##Calculates the Total Number of Units For each County
##Filters to only account for the next 5 years based on Sys.Date() and calculates the percentage of units in those 5 years divided by the Total number of units
USDADataSum<- USDADataSum %>%
  group_by(State_County_FIPS_Code,Year)%>%
  summarise(Units = sum(Project_Size, na.rm=TRUE))%>%
  ungroup%>%
  group_by(State_County_FIPS_Code)%>%
  mutate(TotalUnits = sum(Units))%>%
  filter(Year<= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")) +5 & Year>= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")))%>%
  mutate(USDAsec515Lost5Year = sum(Units))%>%
  mutate(PercentUSDAsec515Lost5Year = USDAsec515Lost5Year/TotalUnits *100)%>%
  select(State_County_FIPS_Code,PercentUSDAsec515Lost5Year)%>%  ##Only Care About Final Percentage Lost of 5 Year
  distinct() ##Used To Remove Duplicate Rows

##Joining the 2020 Census Data with Data of Total Units by County 
##Turns it into a Percent of Units by Total Households
Overall <- merge(House2020,USDASum, by.x = "GEOID", by.y = "State_County_FIPS_Code", all.x=TRUE) %>%
  mutate(Percent_515Properties = TotalUnits/value *100)

##Joins Previous Data with Data of Percent Loss over 5 Years
##Sets Percent Loss over 5 years to Zero if it has no Value (No Projects Ending in that county in 5 years)
Overall <- merge(Overall,USDADataSum, by.x = "GEOID", by.y = "State_County_FIPS_Code", all.x=TRUE)%>%
  mutate(PercentUSDAsec515Lost5Year  = ifelse(is.na(PercentUSDAsec515Lost5Year),0,PercentUSDAsec515Lost5Year))

##Only Wants to have Fips Code, and 2 indicator Values
##This should be the cleaned final form
Output <- Overall %>%
  select(FipsCode = GEOID,Percent_515Properties,PercentUSDAsec515Lost5Year)

##Saves Clean Data to File
##row.names NEEDS to be false
##otherwise First Column will not be Fips Code
write.csv(Output, "Data\\CleanData\\Ready_USDA_Section515.csv",row.names = FALSE)

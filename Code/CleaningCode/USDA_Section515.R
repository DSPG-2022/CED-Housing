library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)


USDAData <- read_csv("Data\\RawData\\USDA\\USDA_Section515.csv", col_names  = TRUE)

USDAData <-USDAData %>%
  filter(substr(State_County_FIPS_Code,0,2)==19)
USDASum <- USDAData%>%
  group_by(State_County_FIPS_Code)%>%
  summarise(TotalUnits = sum(Project_Size))

House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)
USDADataSum <-USDAData %>%
  mutate(Year=ifelse(is.na(Date_Restrictive_Clause_Expires),8888,substr(Date_Restrictive_Clause_Expires,nchar(Date_Restrictive_Clause_Expires)-3,nchar(Date_Restrictive_Clause_Expires))))

USDADataSum<- USDADataSum %>%
  group_by(State_County_FIPS_Code,Year)%>%
  summarise(Units = sum(Project_Size, na.rm=TRUE))%>%
  ungroup%>%
  group_by(State_County_FIPS_Code)%>%
  mutate(TotalUnits = sum(Units))%>%
  filter(Year<= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")) +5 & Year>= as.numeric(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y")))%>%
  mutate(USDAsec515Lost5Year = sum(Units))%>%
  mutate(PercentUSDAsec515Lost5Year = USDAsec515Lost5Year/TotalUnits *100)%>%
  select(State_County_FIPS_Code,PercentUSDAsec515Lost5Year)%>%
  distinct()


Overall <- merge(House2020,USDASum, by.x = "GEOID", by.y = "State_County_FIPS_Code", all.x=TRUE) %>%
  mutate(Percent_515Properties = TotalUnits/value *100)
Overall <- merge(Overall,USDADataSum, by.x = "GEOID", by.y = "State_County_FIPS_Code", all.x=TRUE)%>%
  mutate(PercentUSDAsec515Lost5Year  = ifelse(is.na(PercentUSDAsec515Lost5Year),0,PercentUSDAsec515Lost5Year))
Output <- Overall %>%
  select(FipsCode = GEOID,Percent_515Properties,PercentUSDAsec515Lost5Year)

write.csv(Output, "Data\\CleanData\\Indicator_USDA_Section515.csv",row.names = FALSE)

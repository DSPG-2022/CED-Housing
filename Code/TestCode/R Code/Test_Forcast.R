library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##The Raw Data Files for Assisted Units
LIHTC <- read_csv("Data\\RawData\\HUD\\LIHTC\\LIHTC_AssistedUnits.csv", col_names  = TRUE)
HUD <- read_csv("Data\\RawData\\HUD\\HUD_multiFamAssistUnits.csv", col_names  = TRUE)
USDAData <- read_csv("Data\\RawData\\USDA\\USDA_Section515.csv", col_names  = TRUE)

##File For Matching Fips Code with County Name
County <- read_csv("Data\\Iowa_County_FipsCode.csv")

##Filters for only Data in State of Iowa
##Calculates the Number of Units that will have Initial Compliance Expiring for each given year (15 Years from Date Placed in Service)
##Calculates the Total Number of Units For each County
##YEAR is 8888 because of HUD set it that way for years not stated
LIHTCSum <- LIHTC %>%
  filter(STD_ST == "IA")%>%
  mutate(Year= YR_PIS)%>%
  group_by(COUNTY_LEVEL,Year)%>%
  summarise(LI = sum(LI_UNITS, na.rm=TRUE))%>%
  ungroup%>%
  group_by(COUNTY_LEVEL)%>%
  mutate(TotalLI = sum(LI))%>%
  ungroup%>%
  mutate(PercentLILost = LI/TotalLI * 100)%>%
  mutate(YearEnd = ifelse(Year==8888,Year,Year+15))%>%
  filter(!is.na(COUNTY_LEVEL))

##Filters for only Data in State of Iowa
HudMultiData2 <- HUD %>%
  filter(STD_ST == "IA")

##Calculates the Number of Units that will have Contract Expiring for each given year
##Calculates the Total Number of Units For each County
##YEAR is 8888 because of HUD set it that way for years not stated in LIHTC in want to be consistent
HudMultiData2<- HudMultiData2 %>%
  mutate(Year = 2000+ as.numeric(substr(EXPIRATION_DATE1,nchar(EXPIRATION_DATE1)-1,nchar(EXPIRATION_DATE1))))%>%
  group_by(COUNTY_LEVEL,Year)%>%
  summarise(Units = sum(TOTAL_ASSISTED_UNIT_COUNT, na.rm=TRUE))%>%
  ungroup%>%
  group_by(COUNTY_LEVEL)%>%
  mutate(TotalUnits = sum(Units))%>%
  ungroup%>%
  mutate(PercentMultFamAsstLost = Units/TotalUnits * 100)


##Filters for only Data in State of Iowa
##Creates a Column for Year based on the year given in the Date_Restrictive_Clause_Expires
##If No Year is given, assign it to 8888 to keep consistent with HUD LIHTC year naming scheme
USDADataSum <-USDAData %>%
  filter(substr(State_County_FIPS_Code,0,2)==19)%>%
  mutate(Year=ifelse(is.na(Date_Restrictive_Clause_Expires),8888,substr(Date_Restrictive_Clause_Expires,nchar(Date_Restrictive_Clause_Expires)-3,nchar(Date_Restrictive_Clause_Expires))))

##Calculates the Number of Units that will have Restrictive Clause Expiring for each given year
##Calculates the Total Number of Units For each County
USDADataSum<- USDADataSum %>%
  group_by(State_County_FIPS_Code,Year)%>%
  summarise(Units = sum(Project_Size, na.rm=TRUE))%>%
  ungroup%>%
  group_by(State_County_FIPS_Code)%>%
  mutate(TotalUnits = sum(Units))%>%
  ungroup%>%
  mutate(PercentUSDA515Lost = Units/TotalUnits * 100)

##Selecting only the Valuable Info
##Keeping Column Names constant for each Dataset
##Long Format to rbind with each other
LIHTCData <-LIHTCSum%>%
  mutate(Indicator= "PercentLILost")%>%
  select(Fips = COUNTY_LEVEL,year=  YearEnd,Indicator, Value = PercentLILost)
HUDData <-HudMultiData2%>%
  mutate(Indicator= "PercentMultFamAsstLost")%>%
  select(Fips = COUNTY_LEVEL,year=  Year,Indicator, Value = PercentMultFamAsstLost)
USDa515Data <-USDADataSum%>%
  mutate(Indicator= "PercentUSDA515Lost")%>%
  select(Fips = State_County_FIPS_Code,year=  Year,Indicator, Value = PercentUSDA515Lost)


##Combines all AssistedUnit Loss Together
df <- data.frame()
df <- rbind(df,LIHTCData)
df <- rbind(df,HUDData)
df <- rbind(df,USDa515Data)

##Adds County Names onto Dataset
Over4 <- merge(df,County, by.y= "fips", by.x = "Fips", all.x=TRUE, all.y=TRUE)

##Saves to File
##CHANGE LOCATION ONCED FIGURED OUT
write.csv(Over4, "R\\Cory\\UnitLossPercentbyYear.csv")

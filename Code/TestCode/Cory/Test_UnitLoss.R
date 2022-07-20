library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##The Raw Data Files for Assisted Units
LIHTC <- read_csv("Data\\RawData\\HUD\\LIHTC\\LIHTC_AssistedUnits.csv", col_names  = TRUE)
HUD <- read_csv("Data\\RawData\\HUD\\HUD_multiFamAssistUnits.csv", col_names  = TRUE)
USDAData <- read_csv("Data\\RawData\\USDA\\USDA_Section515.csv", col_names  = TRUE)

County <- read_csv("Data\\Iowa_County_FipsCode.csv")
USDADataSum <-USDAData %>%
  filter(substr(State_County_FIPS_Code,0,2)==19)%>%
  mutate(Year=ifelse(is.na(Date_Restrictive_Clause_Expires),8888,substr(Date_Restrictive_Clause_Expires,nchar(Date_Restrictive_Clause_Expires)-3,nchar(Date_Restrictive_Clause_Expires))))

HudMultiData2 <- HUD %>%
  filter(STD_ST == "IA")

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


  


Over <- merge(LIHTCSum,County, by.y= "fips", by.x = "COUNTY_LEVEL", all.x=TRUE, all.y=TRUE) %>%
  select(COUNTY_LEVEL,county_name,YearEnd,PercentLILost)
#write.csv(Over,"R\\Cory\\LIHTCLostPercentbyYear.csv")

HudMultiData2<- HudMultiData2 %>%
  mutate(Year = 2000+ as.numeric(substr(EXPIRATION_DATE1,nchar(EXPIRATION_DATE1)-1,nchar(EXPIRATION_DATE1))))%>%
  group_by(COUNTY_LEVEL,Year)%>%
  summarise(Units = sum(TOTAL_ASSISTED_UNIT_COUNT, na.rm=TRUE))%>%
  ungroup%>%
  group_by(COUNTY_LEVEL)%>%
  mutate(TotalUnits = sum(Units))%>%
  ungroup%>%
  mutate(PercentMultFamAsstLost = Units/TotalUnits * 100)
Over2 <- merge(HudMultiData2,County, by.y= "fips", by.x = "COUNTY_LEVEL", all.x=TRUE, all.y=TRUE) %>%
  select(COUNTY_LEVEL,county_name,Year,PercentMultFamAsstLost)
#write.csv(Over2,"R\\Cory\\HUDMultiLostPercentbyYear.csv")


USDADataSum<- USDADataSum %>%
  group_by(State_County_FIPS_Code,Year)%>%
  summarise(Units = sum(Project_Size, na.rm=TRUE))%>%
  ungroup%>%
  group_by(State_County_FIPS_Code)%>%
  mutate(TotalUnits = sum(Units))%>%
  ungroup%>%
  mutate(PercentUSDA515Lost = Units/TotalUnits * 100)
Over3 <- merge(USDADataSum,County, by.y= "fips", by.x = "State_County_FIPS_Code", all.x=TRUE, all.y=FALSE)
#write.csv(Over3,"R\\Cory\\USDALostPercentbyYear.csv")

LIHTCData <-LIHTCSum%>%
  mutate(Indicator= "PercentLILost")%>%
  select(Fips = COUNTY_LEVEL,year=  YearEnd,Indicator, Value = PercentLILost)
HUDData <-HudMultiData2%>%
  mutate(Indicator= "PercentMultFamAsstLost")%>%
  select(Fips = COUNTY_LEVEL,year=  Year,Indicator, Value = PercentMultFamAsstLost)
USDa515Data <-USDADataSum%>%
  mutate(Indicator= "PercentUSDA515Lost")%>%
  select(Fips = State_County_FIPS_Code,year=  Year,Indicator, Value = PercentUSDA515Lost)

df <- data.frame()
df <- rbind(df,LIHTCData)
df <- rbind(df,HUDData)
df <- rbind(df,USDa515Data)
Over4 <- merge(df,County, by.y= "fips", by.x = "Fips", all.x=TRUE, all.y=TRUE)
write.csv(Over4, "R\\Cory\\UnitLossPercentbyYear.csv")




Evict<- read_csv("Data\\CleanData\\Ready_Legalaid_AverageEvictionFilings-2020-22.csv")
Evict<-Evict%>%
  mutate(AverageEvictionFilingsPer1000Households = AverageEvictionFilings/OccupiedHouseholds*1000)%>%
  select(FIPS,AverageEvictionFilingsPer1000Households)

write.csv(Evict,"Data\\CleanData\\Ready_Legalaid_AverageEvictionFilings-2020-22.csv",row.names = FALSE)

USPS <-  read_csv("Data\\CleanData\\Ready_USPS_12_2021_Vacancy.csv")

USPSData<- USPS%>%
  group_by(FIPS)%>%
  summarise(Count=sum(TotalResidentialAddresses),TotalVacant=sum(VacantResidentialAddresses),TotalVacant36Month=sum(vac_36_res), AverageDaysAddressesVacant=mean(AverageDaysAddressesVacant))
USPSData<-USPSData%>%
  mutate(TotalPercentVacant = TotalVacant/Count*100, PercentVacant36Month  = TotalVacant36Month/Count*100)%>%
  select(FIPS,TotalPercentVacant,PercentVacant36Month)

write.csv(USPSData,"Data\\CleanData\\Ready_USPS_12_2021_Vacancy.csv",row.names = FALSE)


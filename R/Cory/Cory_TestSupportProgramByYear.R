library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)


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
unique(LIHTCSum$YearEnd)

Over <- merge(LIHTCSum,County, by.y= "fips", by.x = "COUNTY_LEVEL", all.x=TRUE, all.y=FALSE)
write.csv(Over,"R\\Cory\\LIHTCLostPercentbyYear.csv")


HudMultiData2<- HudMultiData2 %>%
  mutate(Year = 2000+ as.numeric(substr(EXPIRATION_DATE1,nchar(EXPIRATION_DATE1)-1,nchar(EXPIRATION_DATE1))))%>%
  group_by(COUNTY_LEVEL,Year)%>%
  summarise(Units = sum(TOTAL_ASSISTED_UNIT_COUNT, na.rm=TRUE))%>%
  ungroup%>%
  group_by(COUNTY_LEVEL)%>%
  mutate(TotalUnits = sum(Units))%>%
  ungroup%>%
  mutate(PercentMultFamAsstLost = Units/TotalUnits * 100)
Over2 <- merge(HudMultiData,County, by.y= "fips", by.x = "COUNTY_LEVEL", all.x=TRUE, all.y=FALSE)
write.csv(Over2,"R\\Cory\\HUDMultiLostPercentbyYear.csv")


USDADataSum<- USDADataSum %>%
  group_by(State_County_FIPS_Code,Year)%>%
  summarise(Units = sum(Project_Size, na.rm=TRUE))%>%
  ungroup%>%
  group_by(State_County_FIPS_Code)%>%
  mutate(TotalUnits = sum(Units))%>%
  ungroup%>%
  mutate(PercentUSDA515Lost = Units/TotalUnits * 100)
Over3 <- merge(USDADataSum,County, by.y= "fips", by.x = "State_County_FIPS_Code", all.x=TRUE, all.y=FALSE)
write.csv(Over3,"R\\Cory\\USDALostPercentbyYear.csv")


u <- unique(Over$county_name)
t <-unique(Over2$county_name)
s <- unique(Over3$county_name)



word = "Crawford%20County"
word<-gsub("%20", " ",word)
plot_ly(
  
)
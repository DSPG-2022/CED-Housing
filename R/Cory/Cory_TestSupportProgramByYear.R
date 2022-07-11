library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)


LIHTC <- read_csv("Data\\RawData\\HUD\\LIHTC\\LIHTC_AssistedUnits.csv", col_names  = TRUE)
HUD <- read_csv("Data\\RawData\\HUD\\HUD_multiFamAssistUnits.csv", col_names  = TRUE)
USDAData <- read_csv("Data\\RawData\\USDA\\USDA_Section515.csv", col_names  = TRUE)

USDADataSum <-USDAData %>%
  filter(substr(State_County_FIPS_Code,0,2)==19)%>%
  mutate(Year=ifelse(is.na(Date_Restrictive_Clause_Expires),8888,substr(Date_Restrictive_Clause_Expires,nchar(Date_Restrictive_Clause_Expires)-3,nchar(Date_Restrictive_Clause_Expires))))

HudMultiData <- HUD %>%
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
write.csv(LIHTCSum,"R\\Cory\\LIHTCLostPercentbyYear.csv")

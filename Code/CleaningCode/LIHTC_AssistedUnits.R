library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)



LIHTC <- read_csv("Data\\RawData\\HUD\\LIHTC\\LIHTC_AssistedUnits.csv", col_names  = TRUE)
House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)

LIHTCSum <- LIHTC %>%
  filter(STD_ST == "IA")%>%
  group_by(COUNTY_LEVEL)%>%
  #ok. Also, there are 2 different values, LI_UNIT and LI_UNITR. LI_UNIT shows the number of low-income units (some projects are NA) and LI_UNITR is the same value as LI_UNIT, but shows the total number of units if LI_UNIT is NA, which valu
  summarise(LI = sum(LI_UNITS, na.rm =TRUE))

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
  select(COUNTY_LEVEL,PercentLILost5Year)%>%
  distinct()


Overall <- merge(House2020,LIHTCSum, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE) %>%
  mutate(PercentLIHTCperHousehold = LI/value*100)
Overall <- merge(Overall,LIHTCSum2, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE)%>%
  mutate(PercentLILost5Year  = ifelse(is.na(PercentLILost5Year),0,PercentLILost5Year))
Output <- Overall %>%
  select(FipsCode =  GEOID,PercentLIHTCperHousehold,PercentLILost5Year)

write.csv(Output, "Data\\CleanData\\Indicator_LIHTC_AssistedUnits.csv", row.names = FALSE)


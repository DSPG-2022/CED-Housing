library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)



HUD <- read_csv("Data\\RawData\\HUD\\HUD_multiFamAssistUnits.csv", col_names  = TRUE)

House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)

MultiData <- HUD %>%
  filter(STD_ST == "IA")
MultiDataSum <- MultiData %>%
  group_by(COUNTY_LEVEL)%>%
  summarise(TotalAssisted = sum(TOTAL_ASSISTED_UNIT_COUNT,na.rm =TRUE))
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
  select(COUNTY_LEVEL,PercentHudSec8Lost5Year)%>%
  distinct()

Overall <- merge(House2020,MultiDataSum, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE) %>%
  mutate(PercentMultiFamAssistPerHouseold = TotalAssisted/value *100)
Overall <- merge(Overall,HudMultiData, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE)%>%
  mutate(PercentHudSec8Lost5Year  = ifelse(is.na(PercentHudSec8Lost5Year),0,PercentHudSec8Lost5Year))
Output <- Overall %>%
  select(FipsCode=GEOID,PercentMultiFamAssistPerHouseold,PercentHudSec8Lost5Year)

write.csv(Output, "Data\\CleanData\\Indicator_HUD_multiFamAssistUnits.csv", row.names = FALSE)

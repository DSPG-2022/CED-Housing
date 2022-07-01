library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)



HUD <- read_csv("Data\\RawData\\HUD\\HUD_HCV.csv", col_names  = TRUE)

House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)

MultiData <- HUD %>%
  filter(STATE == 19)
MultiDataSum <- MultiData %>%
  group_by(COUNTY)%>%
  summarise(Total_HCV = sum(HCV_PUBLIC,na.rm =TRUE))%>%
  ungroup%>%
  mutate(COUNTY = paste("19",COUNTY,sep=""))




Overall <- merge(House2020,MultiDataSum, by.x = "GEOID", by.y = "COUNTY", all.x=TRUE)%>%
  mutate(HCVPercentPerHouseHold = Total_HCV/value *100)

Output <- Overall %>%
  select(NAME,HCVPercentPerHouseHold)

write.csv(Output, "Data\\CleanData\\Indicator_HUD_HCV.csv", row.names = FALSE)

library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)



USDA <- read_csv("Data\\RawData\\HUD\\HUD_multiFamAssistUnits.csv", col_names  = TRUE)

House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)

MultiData <- USDA %>%
  filter(STD_ST == "IA")
MultiDataSum <- MultiData %>%
  group_by(COUNTY_LEVEL)%>%
  summarise(TotalAssisted = sum(TOTAL_ASSISTED_UNIT_COUNT,na.rm =TRUE))


Overall <- merge(House2020,MultiDataSum, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE) %>%
  mutate(Percent = TotalAssisted/value)

Output <- Overall %>%
  select(NAME,Percent)

write.csv(Output, "Data\\CleanData\\Indicator_HUD_multiFamAssistUnits.csv", row.names = FALSE)

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

Overall <- merge(House2020,USDASum, by.x = "GEOID", by.y = "State_County_FIPS_Code", all.x=TRUE) %>%
  mutate(Percent_515Properties = TotalUnits/value *100)%>%
  select(FipsCode = GEOID,Percent_515Properties)

write.csv(Overall, "Data\\CleanData\\Indicator_USDA_Section515.csv",row.names = FALSE)
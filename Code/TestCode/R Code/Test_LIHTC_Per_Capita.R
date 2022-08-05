library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(ggmap)
library(matrixStats)


HUDdata <- read_csv("Low-Income_Housing_Tax_Credit_Properties.csv", col_names  = TRUE)
  

vars <- load_variables(2020,"pl")
view(vars)

House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)

HUDdataSum <- HUDdata %>%
  filter(STD_ST == "IA")%>%
  group_by(COUNTY_LEVEL)%>%
  #ok. Also, there are 2 different values, LI_UNIT and LI_UNITR. LI_UNIT shows the number of low-income units (some projects are NA) and LI_UNITR is the same value as LI_UNIT, but shows the total number of units if LI_UNIT is NA, which valu
  summarise(LI = sum(LI_UNITS, na.rm =TRUE))

Overall <- merge(House2020,HUDdataSum, by.x = "GEOID", by.y = "COUNTY_LEVEL", all.x=TRUE) %>%
  mutate(Percent = LI/value)

Output <- Overall %>%
  select(GEOID,NAME,Percent)

write.csv(Output, "LITHCPerCapita.csv")

library(tidyverse)

# 5-year percentage change in the ZHVI-All Homes 
# Smoothed Seasonally Adjusted Time Series, comparing 
# the most recent month available to the same month five years ago.
pct_change <- read_csv("./Data/RawData/Zillow/All Homes - Smoothed by County.csv") %>%
  filter(StateName == "IA") %>%
  transmute(CountyName = RegionName, Jun17 = `2017-05-31`, Jun20 = `2022-05-31`,
            PctChange = (Jun20 - Jun17) / Jun20 * 100) %>%
  arrange(CountyName)
View(pct_change)

write.csv(pct_change, "Data/CleanData/Indicator_ZillowPctChange.csv")


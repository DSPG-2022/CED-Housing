library(tidyverse)

# 5-year percentage change in the ZHVI-All Homes 
# Smoothed Seasonally Adjusted Time Series, comparing 
# the most recent month available to the same month five years ago.
pct_change <- read_csv("./Data/RawData/Zillow/All Homes - Smoothed by County.csv") %>%
  filter(StateName == "IA") %>%
  transmute(FIPS = str_c(StateCodeFIPS, MunicipalCodeFIPS),
            ZillowHousePriceIndex = (`2022-05-31` - `2017-05-31`) / `2022-05-31` * 100) %>%
  arrange(FIPS)

View(pct_change)

write.csv(pct_change, "Data/CleanData/Ready_ZillowPctChange.csv",
          row.names = F)








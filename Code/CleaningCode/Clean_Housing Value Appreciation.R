library(tidyverse)
library(readxl)
library(lubridate)

HPIPctChange <- read_xlsx("./Data/RawData/FHFA/Counties Developmental Index.xlsx") %>%
  filter(State == "IA") %>%
  select(FIPS = `FIPS code`, Year,
         HPI) %>%
  filter(Year %in% c("2016", "2021")) %>%
  group_by(FIPS) %>%
  pivot_wider(c(FIPS), names_from = Year,
              values_from = HPI) %>%
  mutate(`2016` = as.double(`2016`), `2021` = as.double(`2021`),
    `5YearPctChange` = (`2021` - `2016`) / `2016` * 100) %>%
  select(FIPS, `5YearPctChange`)

# 2016 HPI is missing for Adams, calculated using the 2017-2021 HPI pct change instead
HPIPctChange[2:2,2] <- 27.8

View(HPIPctChange)

write.csv(HPIPctChange, "Data/CleanData/Indicator_HPIPctChange.csv",
          row.names = F)

library(tidyverse)
library(readxl)
library(lubridate)

HPIPctChange <- read_xlsx("./Data/RawData/FHFA/Counties Developmental Index.xlsx") %>%
  filter(State == "IA") %>%
  select(FIPS = `FIPS code`, Year,
         HPI) %>%
  filter(Year %in% c("2017", "2021")) %>%
  group_by(FIPS) %>%
  pivot_wider(c(FIPS), names_from = Year,
              values_from = HPI) %>%
  mutate(`2017` = as.double(`2017`), `2021` = as.double(`2021`),
    `5YearPctChange` = (`2021` - `2017`) / `2017` * 100) %>%
  select(FIPS, `5YearPctChange`)
View(HPIPctChange)

write.csv(HPIPctChange, "Data/CleanData/Indicator_HPIPctChange.csv",
          row.names = F)

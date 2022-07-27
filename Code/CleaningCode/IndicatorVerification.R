# (1) Find the average number of filings (observations) 
# per county in 2019, 2020, and 2021, then divide that 
# average by the number of households in the county based 
# on 2020 Census data

# 2) Find the percentage change in the number of evictions 
# from 2019 to 2021.

library(tidyverse)
library(readxl)
library(tidycensus)

# look for the file Data/RawData/MonthlyEvictionFilings.xlsx; path was invalid
# filings19orig <- read_excel(file.choose(), sheet = "2019")
# filings20orig <- read_excel(file.choose(), sheet = "2020")
# filings21orig <- read_excel(file.choose(), sheet = "2021")
filings19 <- filings19orig 
filings20 <- filings20orig
filings21 <- filings21orig
counties <- read_excel(file.choose())
rented_houses <- get_acs(
  geography = "county", 
  variables = c(rented_houses = "S2502_C05_001E"), 
  state = "IA",
  year = 2020
) %>% 
  select(rented_houses = estimate, FIPS = GEOID) 

filings19$COUNTY <- tolower(filings19$COUNTY) 
cases19 <- filings19 %>%
  group_by(COUNTY) %>%
  unique() %>%
  count() %>% 
  arrange(COUNTY) %>%
  rename(Evictions2019 = n)
cases19$COUNTY <- str_to_title(cases19$COUNTY) 

filings20$COUNTY <- tolower(filings20$COUNTY) 
cases20 <- filings20 %>%
  group_by(COUNTY) %>%
  unique() %>%
  count() %>% 
  arrange(COUNTY)%>%
  rename(Evictions2020 = n)
cases20$COUNTY <- str_to_title(cases20$COUNTY) 

filings21$County <- tolower(filings21$County) 
cases21 <- filings21 %>%
  group_by(County)  %>%
  count() %>% 
  arrange(County) %>%
  rename(COUNTY = County)%>%
  rename(Evictions2021 = n)
cases21$COUNTY <- str_to_title(cases21$COUNTY)

rented_houses$COUNTY = houses$county_name[1:99]
evictions19and20 <- merge(cases19, cases20, by.x= "COUNTY", all.x = T, all.y = T)
allevictions <- merge(evictions19and20, cases21, by.x= "COUNTY", all.x = T, all.y = T)
final.df <- merge(allevictions, rented_houses, by.x = "COUNTY", all.x = T, all.y = T) %>% 
  arrange(COUNTY) %>%
  mutate(AverageEvictionFilingsPer1000Households = ((Evictions2019 + Evictions2020+ Evictions2021)/3)/rented_houses * 1000,
         EvictionFilings2021.2019 = ((Evictions2021 / Evictions2019) - 1)* 100)

task1 <- final.df %>%
  select(COUNTY, AverageEvictionFilingsPer1000Households)

task2 <- final.df %>%
  select(COUNTY, EvictionFilings2021.2019)

write.csv(task1, "Data/CleanData/Indicator_AverageEvictionFilingsPer1000Households.csv",row.names = F)
write.csv(task2, "Data/CleanData/Indicator_EvictionFilings2021.2019.csv",row.names = F)



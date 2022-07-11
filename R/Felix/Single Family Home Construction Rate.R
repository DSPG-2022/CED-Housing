library(readxl)
library(tidyverse)
library(readxl)
library(readr)
library(ggmap)

### Reading in Building Permits data 
permits17 <- read_csv("./Data/RawData/USCB/Building Permits Survey/co2017a.txt") %>% filter(FIPS...2 == "19") %>%
  rename(Bldgs1 = ...7, Units1 = `1-unit`, Value1 =...9,
         Bldgs2 = ...10, Units2 = `2-units`, Value2 =...12,
         Bldgs3.4 = ...13, Units3.4 = `3-4 units`, Value3.4 =...15,
         Bldgs5 = ...16, Units5 = `5+ units`, Value5 =...18)

permits18 <- read_csv("./Data/RawData/USCB/Building Permits Survey/co2018a.txt") %>% filter(FIPS...2 == "19") %>%
  rename(Bldgs1 = ...7, Units1 = `1-unit`, Value1 =...9,
         Bldgs2 = ...10, Units2 = `2-units`, Value2 =...12,
         Bldgs3.4 = ...13, Units3.4 = `3-4 units`, Value3.4 =...15,
         Bldgs5 = ...16, Units5 = `5+ units`, Value5 =...18)

permits19 <- read_csv("./Data/RawData/USCB/Building Permits Survey/co2019a.txt") %>% filter(FIPS...2 == "19") %>%
  rename(Bldgs1 = ...7, Units1 = `1-unit`, Value1 =...9,
         Bldgs2 = ...10, Units2 = `2-units`, Value2 =...12,
         Bldgs3.4 = ...13, Units3.4 = `3-4 units`, Value3.4 =...15,
         Bldgs5 = ...16, Units5 = `5+ units`, Value5 =...18)

permits20 <- read_csv("./Data/RawData/USCB/Building Permits Survey/co2020a.txt") %>% filter(FIPS...2 == "19") %>%
  rename(Bldgs1 = ...7, Units1 = `1-unit`, Value1 =...9,
         Bldgs2 = ...10, Units2 = `2-units`, Value2 =...12,
         Bldgs3.4 = ...13, Units3.4 = `3-4 units`, Value3.4 =...15,
         Bldgs5 = ...16, Units5 = `5+ units`, Value5 =...18)

permits21 <- read_csv("./Data/RawData/USCB/Building Permits Survey/co2021a.txt") %>% filter(FIPS...2 == "19") %>%
  rename(Bldgs1 = ...7, Units1 = `1-unit`, Value1 =...9,
         Bldgs2 = ...10, Units2 = `2-units`, Value2 =...12,
         Bldgs3.4 = ...13, Units3.4 = `3-4 units`, Value3.4 =...15,
         Bldgs5 = ...16, Units5 = `5+ units`, Value5 =...18)

permits_df <- rbind(permits17, permits18, permits19, permits20)
# permits_df$TotalHouses <- housing_df$Total

## reading in houses data
units <- get_acs(
  geography = "county",
  state = "Iowa",
  variables = c(Total = "B25024_001"),
  year = 2020, 
  cache_table = T,
  output = "wide"
)

# Calculate a ratio with the average, annual number of permits 
# for single-family units issued over the last five years (2017-2021) 
# in the numerator and the number of single family units (ACS 2016-20) 
# in the denominator.

permits_df$Units1 <- as.numeric(permits_df$Units1)

## Average number of single family buildings built per county from 2017-2021
# (Building Permits Survey)
sum_Units1_county <- permits_df %>%
  group_by(County) %>%
  select(Units1) %>%
  summarise(sum(Units1)) 

#  Data frame containing the Single Family Home Construction Percentages
SingFamCRate <- data.frame(
  SingleFamHomeConstructPct = sum_Units1_county$`sum(Units1)`/ units$TotalE * 100,
  County = unique(permits_df$County)
  )
View(SingFamCRate)


write.csv(SingFamCRate, "Data/SingFamCRate.csv")


library(readxl)
library(tidyverse)
library(readr)
library(tidycensus)

# Census Bureau Building Permits annual files ("a" files)
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
permits_df$FIPS <- str_c(permits_df$FIPS...2, permits_df$FIPS...3)

# For the number of building permits, sum values in columns 
# reported for 2-unit, 3-4 unit, and 5+ units to obtain a grand 
# total for multi-family units.
# Table B25024 Units in Structure from ACS 2016-20
units <- get_acs(
  geography = "county",
  state = "Iowa",
  variables = c(Total2 = "B25024_004",
                Total3or4 = "B25024_005",
                Total5to9 = "B25024_006",
                Total10to19 = "B25024_007",
                Total20to49 = "B25024_008",
                Total50More = "B25024_009"),
  year = 2020, 
  cache_table = T,
  output = "wide"
) %>%
  mutate(MultiHouseUnits = Total2E + Total3or4E + Total5to9E + 
           Total10to19E + Total20to49E + Total50MoreE)

## Sum of number of multiple family buildings built per county from 2017-2021
# (Building Permits Survey)
permits_df$Units2 <- as.numeric(permits_df$Units2)
permits_df$Units3.4 <- as.numeric(permits_df$Units3.4)
permits_df$Units5 <- as.numeric(permits_df$Units5)
sum_MultiUnits_County <- permits_df %>%
  mutate(MultiUnits = Units2 + Units3.4 + Units5) %>%
  group_by(County) %>%
  summarise(sum(MultiUnits)) 

# Calculate a ratio with the average, annual number of permits 
# for multi-family units issued over the last five years (2017-2021) 
# in the numerator and the number of multi-family units (ACS 2016-20) 
# in the denominator.
MultiFamHomeConstructPct <- sum_MultiUnits_County$`sum(MultiUnits)`/ units$MultiHouseUnits * 100
###
MultFamCRate <- data.frame(
  MultiFamHomeConstructPct,
  FIPS = unique(permits_df$FIPS)
  )
View(MultFamCRate)

write.csv(MultFamCRate, "Data/CleanData/Indicator_MultiFamCRate.csv",
          row.names = F)

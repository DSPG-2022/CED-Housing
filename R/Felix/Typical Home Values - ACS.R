library(readxl)
library(tidyverse)
library(readr)
library(tidycensus)

# Use Table B25077 Median Value (Dollars) from ACS 2016-20
unit_value <- get_acs(
  geography = "county",
  state = "Iowa",
  variables = c(MedianValue = "B25077_001"),
  year = 2020, 
  cache_table = T, 
  output = "wide"
) 
View(unit_value)

library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##Takes in All Data
Overall <- read_csv("Data\\AllCountyData\\OverallDatabase.csv", col_names  = TRUE)


##Converts To Long format
##Keeps Fips, County name in wide
Overall2<- Overall%>%
  pivot_longer(3:ncol(Overall))

##Saves to File
write.csv(Overall2,"Data\\AllCountyData\\OverallDatabaseLong.csv",row.names = FALSE)


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

IndicatorGroup<- read_excel("indicator_definitions_updated.xlsx")

Overall3<- merge(Overall2,IndicatorGroup,by.x="name",by.y= "Name",all.x=TRUE)
##Saves to File
write.csv(Overall3,"Data\\AllCountyData\\OverallDatabaseLong.csv",row.names = FALSE)



library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

Data<- read_excel("non_housing_indicators.xlsx")[1:99,]


MOE <- Data[,endsWith(colnames(Data),"MOE")]
MOE[,"GEOID"] <- Data$FIPS
MOE[,"NAME"] <- Data$`Geographic Area Name`
Data2 <-Data[,!endsWith(colnames(Data),"MOE")] %>%
  select(1,3:17)

write.csv(Data2,"Data\\CleanData\\Ready_non_housing_indicators.csv", row.names=FALSE)

AllMOE <- read_csv("Data\\ALLCountyData\\ACS_MOE_Values.csv")

Output <- merge(AllMOE,MOE,by = "GEOID")%>%
  select(1,Name=  NAME.x,3:18)


write.csv(Output,"Data\\ALLCountyData\\ACS_MOE_Values.csv", row.names=FALSE)

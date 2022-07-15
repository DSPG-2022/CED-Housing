library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(shiny)


Overall <- read_csv("Data\\OverallDatabase.csv", col_names  = TRUE)


OVerallT<- as.data.frame(t(Overall))
Overall2<- Overall%>%
  pivot_longer(3:ncol(Overall))

write.csv(Overall2,"Data\\OverallDatabaseLong.csv",row.names = FALSE)


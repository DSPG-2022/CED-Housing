library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(shiny)


Overall <- read_csv("Data\\OverallDatabase.csv", col_names  = TRUE)%>%
  select(1,3:ncol(Overall))


OVerallT<- as.data.frame(t(Overall))
Overall2<- Overall%>%
  pivot_longer(2:ncol(Overall))



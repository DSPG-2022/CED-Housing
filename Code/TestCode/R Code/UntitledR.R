library(plotly)

library(ggplot2)
library(readxl)
library(tidyverse)
library(readr)

Overall <- read_csv("Data\\AllCountyData\\OverallDatabase.csv", col_names  = TRUE)

Indicators  <- colnames(Overall)
Indicators <- Indicators[3:length(Indicators)]

IndicatorGroup<- read_excel("indicator_definitions.xlsx")


indicator <- Indicators[3]

Index <- which(IndicatorGroup$Name == indicator)

Reverse <- IndicatorGroup$`Higher is better`[Index]


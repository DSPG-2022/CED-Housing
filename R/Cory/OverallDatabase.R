library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##Inputs
inputCSV  = "LITHCPerCapita.csv"
ColumnName = "LIHTC_Per_Capita"


outputCSV = "OverallDatabase.csv"




InputData = read_csv(inputCSV)
OutputData <- read_csv(outputCSV)
colnames(InputData) <- c("GEOID","NAME",ColumnName)

InputData <- InputData%>%
  select(GEOID,ColumnName)
Output = merge(OutputData,InputData, by = "GEOID")

write.csv(Output, outputCSV, row.names = FALSE)

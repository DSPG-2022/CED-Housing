library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)


inputCSV  = "LITHCPerCapita.csv"
ColumnName = "LIHTC_Per_Capita"
outputCSV = "OverallDatabase.csv"

InputData = read_csv(inputCSV)
OutputData <- read_csv(outputCSV)

Output = merge(OutputData,InputData, by = GEOID)
write.csv(InputData, outputCSV, row.names = FALSE)

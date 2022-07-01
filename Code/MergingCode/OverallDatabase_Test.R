library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##Inputs
Files<-list.files("Data\\CleanData")


outputCSV = "Data\\OverallDatabase.csv"
OutputData <- read_csv(outputCSV)
OutputColNames <- colnames(OutputData)
for (file in Files){
  InputData<- read.csv(paste("Data\\CleanData\\",file,sep=""))
  ##if using multi columns, another for loop
  ColumnName = colnames(InputData)[2]
  
  for(name in OutputColNames){
    if(ColumnName == name){
      InputData <- InputData%>% arrange(InputData[,1])
      OutputData <- OutputData%>%arrange(NAME)
      OutputData[,name] <- replace(OutputData[,name],order(OutputData[,1]),InputData[,2])
    }
    else{
      OutputData = merge(OutputData, InputData, by.x = "NAME", by.y = colnames(InputData)[1])
    }
  }
}
write.csv(OutputData, outputCSV, row.names = FALSE)


#order(Output2[,1])
#  replace(Output2[,2],order(Output2[,1]),InputData[,2])
#Output2 <-merge(Output2,InputData,by ="NAME", no.dups= TRUE)
#ColumnName = "LIHTC_Per_Capita"
#outputCSV = "OverallDatabase.csv"

#test<-InputData


#InputData = read_csv(inputCSV)
#OutputData <- read_csv(outputCSV)
#colnames(InputData) <- c("GEOID","NAME",ColumnName)

#InputData <- InputData%>%
#  select(GEOID,ColumnName)
#Output = merge(OutputData,InputData, by = "GEOID")



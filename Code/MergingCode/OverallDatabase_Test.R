library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##Inputs - all the clean data files
## Clean data files should be in the format of 
    #first column : fipscode  
    #Rest of columns, values of indicators for specific fipscode with Indicator being used as column name
Files<-list.files("Data\\CleanData")

##output Datafile
outputCSV = "Data\\OverallDatabase.csv"
OutputData <- read_csv(outputCSV)
OutputColNames <- colnames(OutputData)

##For each code clean Data File
for (file in Files){
  
  ##Gets Seperate Data File
  InputData<- read.csv(paste("Data\\CleanData\\",file,sep=""))
  ##For each indicator in InputData
  for(colIndex in 1:ncol(InputData)){
    ##if not first column **FIRST Column should be fipsCode**
    if(colIndex!= 1){
      
      ##Indicator Name
      ColumnName = colnames(InputData)[colIndex]
      
      inOverall =FALSE
      ##For each Indicator in Overal Dataset
      for(name in OutputColNames){
          ##If the Input indicator matches an indicator in Overal Dataset
          if(ColumnName == name){
            inOverall =TRUE
            ##Find the fipsCodes of input
            InputFipsCodes <- InputData[,1]
          
            ##for each input fipscodes, find matching code in overall dataset
            for (code in InputFipsCodes){
              OutputIndex <- which(OutputData$fips==code)
          
              ##replace Overall Dataset with value from input data
              replace(OutputData[,name],OutputIndex,InputData[which(InputData[,1]==code),colIndex])
            }
          }
        }
      ## If there is no Indicator named that 
      ## Merge Data onto Overall Dataset by fipcode
      if(!inOverall){
        OutputData = merge(OutputData, InputData, by.x = "fips", by.y = colnames(InputData)[1])
        OutputColNames <- colnames(OutputData)
      }
    }
  }
}

##rewrites to OverallDataset
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



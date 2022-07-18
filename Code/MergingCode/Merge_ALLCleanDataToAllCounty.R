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


##output: Datafile
outputCSV = "Data\\AllCountyData\\OverallDatabase.csv"
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
      
      ##value used to determine if data is new indicator being added or new data of the same indicator
      inOverall =FALSE
      
      ##New indicator wants to be merged, whereas updated info should replace old data
      ##assumes input info is most accurate/up-to-date data
      
      ##For each Indicator in Overall Dataset
      for(name in OutputColNames){
        
          ##If the Input indicator matches an indicator in Overall Dataset
          if(ColumnName == name){
            
            ##Indicator is in Old Data
            inOverall =TRUE
            
            ##Find the fipsCodes of input
            InputFipsCodes <- InputData[,1]
          
            ##for each input fipscodes, find matching code in overall dataset
            for (code in InputFipsCodes){
              OutputIndex <- which(OutputData$fips==code)
          
              ##replace Overall Dataset with value from input data
              replace(as.data.frame(OutputData[,name])[OutputIndex,],1,InputData[which(InputData[,1]==code),colIndex])
            }

          }
  
      }
      ##Only want to Add One Indicator to New Data Set at  a time
      ##Otherwise might add all indicators multiples times over
      AddData <- select(InputData,1,colIndex)
      
      ## If there is no Indicator named that 
      ## Merge Data onto Overall Dataset by fipcode
      ## Will Run for each indicator in Input Data
      if(!inOverall){
        OutputData = merge(OutputData, AddData, by.x = "fips", by.y = colnames(InputData)[1])
        OutputColNames <- colnames(OutputData)
      }
    }
  }
}

##rewrites to OverallDataset
##row.names NEEDS to be false
##otherwise First Column will not be Fips Code
write.csv(OutputData, outputCSV, row.names = FALSE)
write.csv(OutputData, "Data\\AllCountyData\\Speedometer\\OverallDatabase", row.names = FALSE)

source("Code\\MergingCode\\Merge_AllCountyDataToLongFormat.R")


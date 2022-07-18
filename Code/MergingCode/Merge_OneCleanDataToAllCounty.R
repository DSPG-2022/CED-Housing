library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##Inputs - Clean Data File You want to select
##Choose DataFile With Fips Code on first Column
File <- file.choose()

##INPUT DATA
InputData<- read.csv(File)


##output: Datafile
outputCSV = "Data\\OverallDatabase.csv"
OutputData <- read_csv(outputCSV)
OutputColNames <- colnames(OutputData)

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

##rewrites to OverallDataset
##row.names NEEDS to be false
##otherwise First Column will not be Fips Code
write.csv(OutputData, outputCSV, row.names = FALSE)




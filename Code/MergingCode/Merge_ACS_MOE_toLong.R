library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

##Takes in All Data
Overall <- read_csv("Data\\AllCountyData\\ACS_MOE_Values.csv", col_names  = TRUE)




MOE <- Overall[,endsWith(colnames(Overall),"M")]
ColMOE <- colnames(MOE)
ColMOE<-substr(ColMOE,0,nchar(ColMOE)-1)
colnames(MOE)<- ColMOE
MOE[,"GEOID"] <- Overall$GEOID
MOE[,"NAME"] <- Overall$Name


MOES <- Overall[,endsWith(colnames(Overall),"MOE")]
ColMOES <- colnames(MOES)
ColMOES<-substr(ColMOES,0,nchar(ColMOES)-4)
ColMOES[2:7]<- paste(ColMOES[2:7],"_EST",sep="")
colnames(MOES)<- ColMOES
MOES[,"GEOID"] <- Overall$GEOID



Overall <- merge(MOE,MOES,by  ="GEOID")

Overall<-Overall%>%
  select(GEOID,Name = NAME,everything())
##Converts To Long format
##Keeps Fips, County name in wide
Overall2<- Overall%>%
  pivot_longer(3:ncol(Overall))


##Saves to File
write.csv(Overall2,"Data\\AllCountyData\\ACS_MOE_ValuesMOE.csv",row.names = FALSE)



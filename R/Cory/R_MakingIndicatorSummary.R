library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(ggmap)
library(matrixStats)

CountyName = "Story"


Data <-read_excel("indicator_summary_table_example.xlsx", col_names =  FALSE)
DataT <- as.data.frame(t(Data))
glimpse((DataT))
df <- data.frame(DataT$V1)
colnames(df) <- "Indicator"
df <- df %>%
  mutate(`State Average` = DataT$V3)

##need to change MOE if on new sheet
MOE <- Data[105:205,]
colnames(MOE) <- Data[1,]

MOE2 <- lapply(MOE[,3:46], as.numeric)
MOE2 <-as.data.frame(MOE2)
MOE2 <- MOE2 %>%
  mutate(County = MOE[,2])%>%
  select(County,everything())%>%
  filter(County == CountyName)
Tmoe <- as.data.frame(t(MOE2))
Tmoe <- rbind(NA,Tmoe)



options(scipen=999)
DataCountyOnlyRow <- Data[1:102,]
DataCountyOnlyColumn <- DataT[3:46,1:102]
DataCountyOnlyColumn$V2 = NULL
DataCountyOnlyColumn$V3 = NULL
DataCountyOnlyColumn2 <- lapply(DataCountyOnlyColumn[2:100], as.numeric)
DataCountyOnlyColumn2 <- as.data.frame(DataCountyOnlyColumn2)
County_Median <- as.data.frame(rowMedians(as.matrix(DataCountyOnlyColumn2), na.rm =TRUE))
colnames(County_Median)<- "CountyMedian"


County_Median <- rbind(c("NA","NA"),County_Median)
County_Median <- rbind(c("NA","NA"),County_Median)

df<- df %>%
  mutate(`County Median` = County_Median$CountyMedian)




colnames(DataT) <- DataT[2,]
#For Story County
SelectedCounty <-DataT%>%
  select(CountyName)

df<- df %>%
  mutate(SelectedCounty = SelectedCounty$Story)



Tmoe <- Tmoe %>%
  mutate(value=  df$SelectedCounty)
Tmoe2 <- Tmoe[-1:-2,]
Tmoe2 <- lapply(Tmoe2, as.numeric)
Tmoe2 <-as.data.frame(Tmoe2)



Tmoe2 <- Tmoe2 %>%
  mutate(rangeV = ifelse(is.na(V1),value,paste(value-V1,"-",value+V1,sep="")))
Tmoe2 <- rbind(NA,Tmoe2)
Tmoe2 <- rbind(NA,Tmoe2)
  
 
df<-df %>%
  mutate(Range = Tmoe2$rangeV)

colnames(DataCountyOnlyRow)<- DataCountyOnlyRow[1,]
DataCountyOnlyRow2<- DataCountyOnlyRow[-1:-3,]

DataCountyOnlyRow2<- DataCountyOnlyRow2[,-1:-2]
DataCountyOnlyRow2 <- lapply(DataCountyOnlyRow2, as.numeric)
DataCountyOnlyRow2 <- as.data.frame(DataCountyOnlyRow2)
quantile(DataCountyOnlyRow2, na.rm=TRUE)

find_quartile <- function(df3){
  df4 <- data.frame()
  for(indicator in colnames(df3)){
    if(!is.na(indicator)){
      quartiles <-quantile(df3[,indicator], na.rm=TRUE)
      val = which( colnames(DataT)==CountyName )[1] -3
      df4 <- rbind(df4, ifelse(df3[val,indicator]>quartiles[4],4,ifelse(df3[val,indicator]>quartiles[3],3,ifelse(df3[val,indicator]>quartiles[2],2,1))))
    }else{
      df4<- rbind(df4,0)
    }
  }
  
  return(df4)
}

Quartile = find_quartile(DataCountyOnlyRow2)
 
Quartile <- rbind(NA,Quartile)   
Quartile <- rbind(NA,Quartile)        

df <- df%>%
  mutate(quartileForCounty = Quartile$X3, MOE = ifelse(!is.na(Tmoe2$V1),Tmoe2$V1,0), Range_L=Tmoe2$value-MOE,Range_H=Tmoe2$value+MOE)
colnames(df)<-c("Indicators","State Average","County Median",CountyName,"Range","Quartile","MOE","Range_L","Range_H")

df<-df[-1:-2,]
##TEST CODE
tdf <- as.data.frame(t(df))
colnames(tdf)<-tdf[1,]
tdf<-tdf[-1,]

write.csv(df,"IndicatorSummary.csv")
write.csv(tdf,"TIndicator.csv")

df <- df%>%
  mutate(quartileForCounty = Quartile$X3, MOE = ifelse(!is.na(Tmoe2$V1),Tmoe2$V1,0), Range_L=Tmoe2$value-MOE,Range_H=Tmoe2$value+MOE)

ldf<-df %>%
  filter(Indicators != "Assets of Public Charities") %>%
  filter(Indicators != "Primary Medical Care Access")%>%
  filter(MOE !=0)
ggplot(ldf, aes(x = Indicators))+
  geom_point(aes(y=Range_L), color= "red",alpha= 0.8)+
  geom_point(aes(y=Range_H), color= "blue" , alpha= 0.8)+
  coord_flip()+
  labs(title = "Of Indicators with Margins of Error, range of values")+
  ylab("Value")+
  geom_text(aes(y = (Range_L+Range_H)/2, label = paste("MOE is " ,round(MOE, digits=2), sep = " "),vjust= -1.2))


ggplot(mdf)+
  geom_col(aes(x=Indicators, y= value),position = position_dodge())+
  coord_flip()

library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
    
##Raw Data File for FMR
  ##Only looking at 2 bedrooms apartment because it is supposed to be most accurate
Data<- read.csv("Data\\RawData\\HUD\\FMR\\FMR_2Bed_1983_2022_rev.csv")

##only care about Countly level data in Iowa
Data<-Data%>%
  mutate(fips = substr(fips2010,0,5))%>%
  filter(state == 19)

##RAW employment data
EmploymentData<-read.csv("Data\\RawData\\BEA\\Raw_Bea_Employment.csv")

##find percent change for a 5 year period
##BEA only has data until 2020, so look at 2016-2020
##Looking at both total employment, and just wage and salaried employment
EmploymentData2 <- EmploymentData%>%
  filter(Description=='Total employment (number of jobs)')%>%
  mutate(PrcntChangeEmployTotal2016_2020 = (X2020/X2016 -1)*100)%>%
  select(GeoFips,PrcntChangeEmployTotal2016_2020)
EmploymentData3 <- EmploymentData%>%
  filter(Description!='Total employment (number of jobs)')%>%
  mutate(PrcntChangeEmployWage2016_2020 = (X2020/X2016 -1)*100)%>%
  mutate(PrcntChangeEmployWage2019_2020 = (X2020/X2019 -1)*100)%>%
  select(GeoFips,PrcntChangeEmployWage2016_2020,PrcntChangeEmployWage2019_2020)

##find same percent change but for fmr Data
Data2<- Data%>%
  mutate(PrcntChangeFMR2017_2021= (fmr21_2/fmr17_2-1)*100)%>%
  mutate(PrcntChangeFMR2016_2020= (fmr20_2/fmr16_2-1)*100)%>%
  mutate(PrcntChangeFMR2020_2022= (fmr22_2/fmr20_2-1)*100)


Data2<-Data2%>%
  select(fips,PrcntChangeFMR2016_2020,PrcntChangeFMR2017_2021,PrcntChangeFMR2020_2022)

##Put them together
Overall<- merge(Data2,EmploymentData2,by.x="fips",by.y="GeoFips")
Overall<- merge(Overall,EmploymentData3,by.x="fips",by.y="GeoFips")

##save to file
write.csv(Overall,"Data\\CleanData\\FMRTest.csv",row.names = FALSE)

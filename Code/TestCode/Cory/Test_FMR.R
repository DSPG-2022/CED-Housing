library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

Data<- read.csv("Data\\RawData\\HUD\\FMR\\FMR_2Bed_1983_2022_rev.csv")

Data<-Data%>%
  mutate(fips = substr(fips2010,0,5))%>%
  filter(state == 19)

EmploymentData<-read.csv("Data\\RawData\\BEA\\Raw_Bea_Employment.csv")

EmploymentData2 <- EmploymentData%>%
  filter(Description=='Total employment (number of jobs)')%>%
  mutate(PrcntChangeEmployTotal2016_2020 = (X2020/X2016 -1)*100)%>%
  select(GeoFips,PrcntChangeEmployTotal2016_2020)
EmploymentData3 <- EmploymentData%>%
  filter(Description!='Total employment (number of jobs)')%>%
  mutate(PrcntChangeEmployWage2016_2020 = (X2020/X2016 -1)*100)%>%
  mutate(PrcntChangeEmployWage2019_2020 = (X2020/X2019 -1)*100)%>%
  select(GeoFips,PrcntChangeEmployWage2016_2020,PrcntChangeEmployWage2019_2020)

Data2<- Data%>%
  mutate(PrcntChangeFMR2017_2021= (fmr21_2/fmr17_2-1)*100)%>%
  mutate(PrcntChangeFMR2016_2020= (fmr20_2/fmr16_2-1)*100)%>%
  mutate(PrcntChangeFMR2020_2022= (fmr22_2/fmr20_2-1)*100)

Data2<-Data2%>%
  select(fips,PrcntChangeFMR2016_2020,PrcntChangeFMR2017_2021,PrcntChangeFMR2020_2022)

Overall<- merge(Data2,EmploymentData2,by.x="fips",by.y="GeoFips")
Overall<- merge(Overall,EmploymentData3,by.x="fips",by.y="GeoFips")

write.csv(Overall,"Data\\CleanData\\FMRTest.csv",row.names = FALSE)

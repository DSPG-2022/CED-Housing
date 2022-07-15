library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

Files<-list.files("Data\\RawData\\HUD\\FMR")

df <- data.frame()
data <- read_csv("Data\\RawData\\HUD\\FMR\\FY2018_4050_Final.csv")
for (file in Files){
  InputData<- read.csv(paste("Data\\RawData\\HUD\\FMR\\",file,sep=""))
  year = substr(file,3,6)
  InputData<- InputData%>%
    filter(state==19)%>%
    mutate(Year = year, county  = substr(fips2010,3,5))%>%
  
    select(fmr0=fmr_0,fmr1=fmr_1,fmr2=fmr_2,fmr3=fmr_3,fmr4=fmr_4,State = state,county,Year)
  df <- rbind(df,InputData)
  
}


write.csv(df,"Data\\RawData\\HUD\\FMR\\FMR2010-2022Combined.csv")



data <- read_csv("Data\\RawData\\HUD\\FMR\\FMR2010-2022Combined.csv")
County <- read_csv("Data\\Iowa_County_FipsCode.csv")
Over3 <- merge(df,County, by.y= "fips", by.x = "fips", all.x=TRUE, all.y=FALSE)
write.csv(Over3,"R\\Cory\\HUDFMR2010_2022.csv")

df<-df%>%
  mutate(fips = 19000+as.numeric(county))

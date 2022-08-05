library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(ggmap)
library(fmsb)


vars <- load_variables(2020,"acs5")
view(vars)


Data <- get_acs(
  survey = "acs5" ,
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(RenterUnits = 'B25106_024', Burden = 'B25070_007', Burden2= "B25070_008", Burden3 = "B25070_009", Burden4 = 'B25070_010'
                , Unemploy = "C18120_006", LaborForce= 'C18120_002',RenterUnitsH = "B25011_026", TotalUnits ='B25033_001',Vacant ="B25002_003", TotalHouses ='B25002_001',
                PlumbingRent ="B25049_007", TotalRental ='B25049_006'),
  year =2020,
  output ="wide"
  
)
Data <- Data %>%
  mutate(
    TotalBurden = BurdenE + Burden2E + Burden3E+ Burden4E,PercentBurden  = TotalBurden/RenterUnitsE*100, MaxBurden = max(PercentBurden),MinBurden = min(PercentBurden),
    PercentUmemploy = UnemployE/LaborForceE *100, MaxPercentUmemploy = max(PercentUmemploy), MinPercentUmemploy = min(PercentUmemploy),
    PercentRent = RenterUnitsE/TotalUnitsE *100,MaxPercentRent = max(PercentRent), MinPercentRent = min(PercentRent),
    PercentVacant = VacantE/TotalHousesE *100, maxPercent = max(PercentVacant),minPercent = min(PercentVacant),
    PercentLackPlumb = PlumbingRentE/TotalRentalE *100, maxPercentPlumb = max(PercentLackPlumb),minPercentPlumb = min(PercentLackPlumb)
  )%>%
  select(NAME,MaxBurden,MinBurden,PercentBurden,MaxPercentUmemploy,MinPercentUmemploy,PercentUmemploy,MaxPercentRent,MinPercentRent,PercentRent,maxPercent,minPercent,PercentVacant,maxPercentPlumb,minPercentPlumb,PercentLackPlumb)

Data2 <- Data %>%
  select(NAME,PercentBurden,PercentUmemploy,PercentRent,PercentVacant,PercentLackPlumb)

maxpercents = max(Data$MaxBurden,Data$MaxPercentUmemploy,Data$MaxPercentRent,Data$maxPercent,Data$maxPercentPlumb)
minpercents = min(Data$MinBurden,Data$MinPercentUmemploy,Data$MinPercentRent,Data$minPercent,Data$minPercentPlumb)


df <- t(data.frame( "Max"= c(maxpercents,maxpercents,maxpercents,maxpercents,maxpercents),
                  "Min"= c(minpercents,minpercents,minpercents,minpercents,minpercents)))

colnames(df) <- c("PercentBurden","PercentUmemploy","PercentRent","PercentVacant","PercentLackPlumb")

newData<- Data2 %>%
  filter(NAME == "Story County, Iowa")%>%
  select(PercentBurden,PercentUmemploy,PercentRent,PercentVacant,PercentLackPlumb)
df <-rbind(df,newData)

rownames(df)[rownames(df)== 1] = "Story County"

newData<- Data2 %>%
  filter(NAME == "Polk County, Iowa")%>%
  select(PercentBurden,PercentUmemploy,PercentRent,PercentVacant,PercentLackPlumb)
df <-rbind(df,newData)

rownames(df)[rownames(df)== 1] = "Polk County"

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart(df, axistype=1,
           #caxislabels
           pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, vlcex=0.8 ,
           caxislabels=seq(0,maxpercents,round(maxpercents/4)),
           title = "Radar Graph in Percentages for 2 different Iowa Counties"
)

legend(x=0.5, y=1, legend = rownames(df[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(ggmap)
library(fmsb)


vars <- load_variables(2020,"acs5")
view(vars)

test <- get_acs(
  survey = "acs5" ,
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(RenterUnits = 'B25106_024', Burden = 'B25070_007', Burden2= "B25070_008", Burden3 = "B25070_009", Burden4 = 'B25070_010'),
  year =2020,
  output ="wide"

)

test <- test %>%
  mutate(TotalBurden = BurdenE + Burden2E + Burden3E+ Burden4E)%>%
  mutate(PercentBurden  = TotalBurden/RenterUnitsE*100) %>%
  mutate(MaxBurden = max(PercentBurden),MinBurden = min(PercentBurden)) %>%
  select(NAME,MaxBurden,MinBurden,PercentBurden)

MedianIncome <- get_acs(
  survey = "acs5" ,
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(MedianIncome = "B21004_001"),
  year =2020,
  output ="wide"
  
)

MedianIncome<- MedianIncome %>%
  mutate(MaxIncome = max(MedianIncomeE), MinIncome = min(MedianIncomeE))
MedianIncome<-MedianIncome  %>%
  select(NAME,MaxIncome,MinIncome,MedianIncomeE)


Unemp <- get_acs(
  survey = "acs5" ,
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(Unemploy = "C18120_006", LaborForce= 'C18120_002'),
  year =2020,
  output ="wide"
  
)
Unemp<- Unemp %>%
  mutate(PercentUmemploy = UnemployE/LaborForceE *100)%>%
  mutate(MaxPercentUmemploy = max(PercentUmemploy), MinPercentUmemploy = min(PercentUmemploy))
Unemp<-Unemp  %>%
  select(NAME,MaxPercentUmemploy,MinPercentUmemploy,PercentUmemploy)



Renter <- get_acs(
  survey = "acs5" ,
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(RenterUnits = "B25011_026", TotalUnits ='B25033_001'),
  year =2020,
  output ="wide"
  
)
Renter<- Renter %>%
  mutate(PercentRent = RenterUnitsE/TotalUnitsE *100)%>%
  mutate(MaxPercentRent = max(PercentRent), MinPercentRent = min(PercentRent))
Renter<-Renter  %>%
  select(NAME,MaxPercentRent,MinPercentRent,PercentRent)

Vacant <- get_acs(
  survey = "acs5" ,
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(Vacant ="B25002_003", TotalHouses ='B25002_001'),
  year =2020,
  output ="wide"
)
Vacant2 <- Vacant %>%
  select(NAME,VacantE,TotalHousesE)%>%
  mutate(PercentVacant = VacantE/TotalHousesE *100, maxPercent = max(PercentVacant),minPercent = min(PercentVacant))%>%
  select(NAME,maxPercent,minPercent,PercentVacant)

Vacant3<-as.data.frame(t(Vacant2[,-1]))
colnames(Vacant3) <- Vacant2$NAME
radarchart(Vacant3)

Plumbing <- get_acs(
  survey = "acs5" ,
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(PlumbingRent ="B25049_007", TotalHouses ='B25049_006'),
  year =2020,
  output ="wide"
)B25049_007

overall <- merge(Vacant2,test, by = "NAME")
overall <- merge(overall,MedianIncome, by = "NAME")
overall <- merge(overall,Unemp, by = "NAME")
overall <- merge(overall,Renter, by = "NAME")

overallCounty <- overall %>%
  filter(NAME == "Story County, Iowa")%>%
  gather(key= NAME)
overallCounty2 <- overall %>%
  filter(NAME == "Polk County, Iowa")%>%
  gather(key= NAME)

PercentVacant  <-c(overallCounty[1,2],overallCounty[2,2],overallCounty[3,2], overallCounty2[3,2])
PercentBurden  <-c(overallCounty[4,2],overallCounty[5,2],overallCounty[6,2],overallCounty2[6,2])
MedianIncome  <-c(overallCounty[7,2],overallCounty[8,2],overallCounty[9,2],overallCounty2[9,2])
Unemploy  <-c(overallCounty[10,2],overallCounty[11,2],overallCounty[12,2],overallCounty2[12,2])
PercentRenter  <-c(overallCounty[13,2],overallCounty[14,2],overallCounty[15,2],overallCounty2[15,2])
StoryCounty <-data.frame(PercentVacant,PercentBurden,MedianIncome,Unemploy,PercentRenter)

rownames(StoryCounty) <-c("Max","Min","Story County","Polk County")


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart(StoryCounty,
          #caxislabels
          pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
          cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, vlcex=0.8 
           )
           
legend(x=0.5, y=1, legend = rownames(StoryCounty[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)





##Example
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
# head(data)

# The default radar chart 
radarchart(data)


set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# plot with default options:
radarchart(data)

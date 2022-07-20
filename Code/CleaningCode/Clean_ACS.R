library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

Data <-read.csv("Data\\RawData\\ACS\\RAW_ACS.csv")

DATA2<- Data[,3:ncol(Data)]
MOE <- DATA2[,endsWith(colnames(DATA2),"M")]
MOE[,"GEOID"] <- Data$GEOID
MOE[,"NAME"] <- Data$NAME

MOE<-MOE%>%
  select(GEOID,NAME,everything())

ACSDATA<- DATA2[,!endsWith(colnames(DATA2),"M")]
ACSColNames <- colnames(ACSDATA)
ACSColNames <- substr(ACSColNames,0,nchar(ACSColNames)-1)

colnames(ACSDATA)<- ACSColNames
ACSDATA[,"GEOID"] <- MOE$GEOID
ACSDATA[,"NAME"] <- MOE$NAME

ACSDATA<-ACSDATA%>%
  select(GEOID,NAME,everything()) 


ACSDATA<-ACSDATA%>%
  group_by(NAME)%>%
  mutate(AffordablityIndex = MedianValueOwner/MedianEarnings, HousingPriceDispersion = UpperQuartileValue/LowerQuartileValue, CostBurdenOwnver=  sum(OwnerCost30_35PIncome,OwnerCostOver35PIncome),
         CostBurdenRenter = sum(RenterCostOver35PIncome,RenterCost30_35PIncome),MutliFamShare = sum(MultiFam2Units,MultiFam20Units,MultiFam5_9Units,MUltiFam3_4Units,MultiFam10_19Units),
         RentalUnitRatio  =sum(ForRent,RenterOCCUPIED,RenderNotOccupied)/OccupiedHousingUnits)
FinalAcs <- ACSDATA %>%
  select(1:2,TypicalRenterCosts = GrossRent, TypicalOwnerCosts= MedianMontlyCostOwner, AgingHousingStock = PercentHouseholdsBuildPre1940,AffordabilityIndex = AffordablityIndex,HousingPriceDispersion,CostBurdenOwner  =CostBurdenOwnver,CostBurdenRenter, HomeOwnerShipRate,RentalUnitRatio,MultiFamShare=MutliFamShare)


write.csv(FinalAcs,"Data\\CleanData\\Ready_ACS.csv")




##AFFORABLILITY INDEX  = MEDIAN VALUE / MEDIAN EARNINGS
##HOUSING PRICE DISPERSION = UPPERQUARTILE/LOWERQUARTILE
##Cost Burdened Owner = SUM OwnerCostsPercent...  MOE = root(moe^2 + moe^2)
##Cost Burdened Renter = SUM RenterCostsPercent...  MOE = root(moe^2 + moe^2)
##MULTI Fam = Sum MultFamUnits...MOE = Root(moe^2+moe^2+moe^2...)
##RentalUnitRatio = SUm forRent, RenterNot Occupied, Renter Occupied, moe
  ##Ratio  total rental units by total housing units

  

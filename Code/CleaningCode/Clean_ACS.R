library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)


##Raw Data
Data <-read.csv("Data\\RawData\\ACS\\RAW_ACS.csv")

##Filter for just the columns with MOE values
DATA2<- Data[,3:ncol(Data)]
MOE <- DATA2[,endsWith(colnames(DATA2),"M")]
MOE[,"GEOID"] <- Data$GEOID
MOE[,"NAME"] <- Data$NAME

##Reorganize
MOE<-MOE%>%
  select(GEOID,NAME,everything())

##Filter for just columns with ACS data
ACSDATA<- DATA2[,!endsWith(colnames(DATA2),"M")]
ACSColNames <- colnames(ACSDATA)
ACSColNames <- substr(ACSColNames,0,nchar(ACSColNames)-1)

colnames(ACSDATA)<- ACSColNames
ACSDATA[,"GEOID"] <- MOE$GEOID
ACSDATA[,"NAME"] <- MOE$NAME

##Reorganize
ACSDATA<-ACSDATA%>%
  select(GEOID,NAME,everything()) 

##Indicator Calculations
ACSDATA<-ACSDATA%>%
  group_by(NAME)%>%
  mutate(AffordablityIndex = MedianValueOwner/MedianEarnings, HousingPriceDispersion = UpperQuartileValue/LowerQuartileValue, CostBurdenOwnver=  sum(OwnerCost30_35PIncome,OwnerCostOver35PIncome),
         CostBurdenRenter = sum(RenterCostOver35PIncome,RenterCost30_35PIncome),MutliFamShare = sum(MultiFam2Units,MultiFam20Units,MultiFam5_9Units,MUltiFam3_4Units,MultiFam10_19Units),
         RentalUnitRatio  =sum(ForRent,RenterOCCUPIED,RenderNotOccupied)/OccupiedHousingUnits)%>%
  ungroup

##Reorginize to Nice Names
FinalAcs <- ACSDATA %>%
  select(GEOID,TypicalRenterCosts = GrossRent, TypicalOwnerCosts= MedianMontlyCostOwner, AgingHousingStock = PercentHouseholdsBuildPre1940,AffordabilityIndex = AffordablityIndex,HousingPriceDispersion,CostBurdenOwner  =CostBurdenOwnver,CostBurdenRenter, HomeOwnerShipRate,RentalUnitRatio,MultiFamShare=MutliFamShare)

##Save to csv
write.csv(FinalAcs,"Data\\CleanData\\Ready_ACS.csv", row.names=FALSE)


##Calucations of MOE for ratios and sums
DATAMoe<- DATA2
DATAMoe[,"NAME"] <- MOE$NAME
DATAMoe<- DATAMoe%>%
  group_by(NAME)%>%
  mutate(x  = (MedianValueOwnerE/MedianEarningsE)^2,y = MedianEarningsM^2)%>%
  mutate(z=x*y,a = z+(MedianValueOwnerM^2))%>%
  mutate(b=sqrt(a),AffordabilityIndexM =b/MedianEarningsE)%>%
  mutate(q  = (UpperQuartileValueE/LowerQuartileValueE)^2,w = LowerQuartileValueM^2)%>%
  mutate(e=q*w,r = e+(UpperQuartileValueM^2))%>%
  mutate(t=sqrt(r),HousingPriceDispersionM =t/LowerQuartileValueE)%>%
  mutate(totalRent=sum(ForRentE,RenterOCCUPIEDE,RenderNotOccupiedE))%>%
  ungroup 
DATAMoe[,"GEOID"] <- MOE$GEOID
DATAMoe<-DATAMoe%>%
  select(GEOID,HousingPriceDispersionM,AffordabilityIndexM,OccupiedHousingUnitsM,OccupiedHousingUnitsE,totalRent )

##Calucations of MOE for ratios and sums
MOE<- merge(MOE,DATAMoe,by="GEOID")
MOEData<- MOE%>%
  group_by(NAME)%>%
  mutate(x = sum(OwnerCostOver35PIncomeM^2,OwnerCost30_35PIncomeM^2),y=sum(RenterCost30_35PIncomeM^2,RenterCostOver35PIncomeM^2))%>%
  mutate(CostBurdenOwnersM = sqrt(x))%>%
  mutate(CostBurdenRenterM= sqrt(y))%>%
  mutate(a = sum(MultiFam2UnitsM^2,MultiFam20UnitsM^2,MUltiFam3_4UnitsM^2,MultiFam5_9UnitsM^2,MultiFam10_19UnitsM^2))%>%
  mutate(MultFamShareM = sqrt(a))%>%
  mutate(b=sum(ForRentM^2,RenterOCCUPIEDM^2,RenderNotOccupiedM^2))%>%
  mutate(TotalRentalUnitsM=sqrt(b))%>%
  mutate(Q = (totalRent/OccupiedHousingUnitsE)^2, W = OccupiedHousingUnitsM.x^2)%>%
  mutate(E=Q*W, L= E + (TotalRentalUnitsM^2))%>%
  mutate(f=sqrt(L), RentalUnitRatioM = f/OccupiedHousingUnitsE)

##Rename Columns
MOEData<- MOEData%>%
  select(GEOID,NAME,AgingHousingStockM =PercentHouseholdsBuildPre1940M, TypicalOwnerCosts=MedianMontlyCostOwnerM,TypicalRenterCosts=GrossRentM,AffordabilityIndexM,HousingPriceDispersionM,CostBurdenOwnersM,CostBurdenRenterM,HomeOwnerShipRateM,MultFamShareM,RentalUnitRatioM)
##Save to csv
write.csv(MOEData,'Data\\AllCountyData\\ACS_MOE_Values.csv',row.names=FALSE)

##AFFORABLILITY INDEX  = MEDIAN VALUE / MEDIAN EARNINGS
##HOUSING PRICE DISPERSION = UPPERQUARTILE/LOWERQUARTILE
##Cost Burdened Owner = SUM OwnerCostsPercent...  MOE = root(moe^2 + moe^2)
##Cost Burdened Renter = SUM RenterCostsPercent...  MOE = root(moe^2 + moe^2)
##MULTI Fam = Sum MultFamUnits...MOE = Root(moe^2+moe^2+moe^2...)
##RentalUnitRatio = SUm forRent, RenterNot Occupied, Renter Occupied, moe
  ##Ratio  total rental units by total housing units

  

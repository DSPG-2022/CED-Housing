library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

vars <- load_variables(2020,"acs5/profile")

ACSData <- get_acs(
  survey = "acs5",
  geography = "county",
  key = CENSUS_API_KEY,
  state = "IA",
  variables = c(PercentHouseholdsBuildPre1940 = "DP04_0026P",GrossRent = "B25064_001",MedianMontlyCostOwner = "B25088_001",MedianValueOwner = "B25077_001", MedianEarnings = 'S2001_C01_013',UpperQuartileValue = 'B25078_001', LowerQuartileValue = 'B25076_001',OwnerCostOver35PIncome = 'DP04_0124P',OwnerCost30_35PIncome= 'DP04_0123P',RenterCostOver35PIncome = 'DP04_0142P',RenterCost30_35PIncome= 'DP04_0140P',HomeOwnerShipRate ='DP04_0046P',MultiFam2Units = "DP04_0009P", MUltiFam3_4Units ="DP04_0011P",MultiFam5_9Units ='DP04_0013P',MultiFam10_19Units ="DP04_0015P",MultiFam20Units = "DP04_0017P",OccupiedHousingUnits ='DP04_0045', ForRent = 'B25004_002', RenderNotOccupied = "B25004_003", RenterOCCUPIED ="DP04_0047"),
  year =2020,
  output="wide"  

)

write.csv(ACSData,"Data\\RawData\\ACS\\Raw_ACS.csv", row.names=FALSE)


##AFFORABLILITY INDEX  = MEDIAN VALUE / MEDIAN EARNINGS
##HOUSING PRICE DISPERSION = UPPERQUARTILE/LOWERQUARTILE
##Cost Burdened Owner = SUM OwnerCostsPercent...  MOE = root(moe^2 + moe^2)
##Cost Burdened Renter = SUM RenterCostsPercent...  MOE = root(moe^2 + moe^2)
##MULTI Fam = Sum MultFamUnits...MOE = Root(moe^2+moe^2+moe^2...)
##RentalUnitRatio = SUm forRent, RenterNot Occupied, Renter Occupied, moe
  ##Ratio  total rental units by total housing units


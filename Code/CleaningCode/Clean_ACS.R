library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)

Data <-read.csv("Data\\RawData\\ACS\\RAW_ACS.csv")



##AFFORABLILITY INDEX  = MEDIAN VALUE / MEDIAN EARNINGS
##HOUSING PRICE DISPERSION = UPPERQUARTILE/LOWERQUARTILE
##Cost Burdened Owner = SUM OwnerCostsPercent...  MOE = root(moe^2 + moe^2)
##Cost Burdened Renter = SUM RenterCostsPercent...  MOE = root(moe^2 + moe^2)
##MULTI Fam = Sum MultFamUnits...MOE = Root(moe^2+moe^2+moe^2...)
##RentalUnitRatio = SUm forRent, RenterNot Occupied, Renter Occupied, moe
##Ratio  total rental units by total housing units

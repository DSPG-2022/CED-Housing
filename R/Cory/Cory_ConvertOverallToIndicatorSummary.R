library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)



Overall <- read_csv("Data\\CleanData\\Indicator_HUD_HCV.csv", col_names  = TRUE)


OVerallT<- as.data.frame(t(Overall))
Overall2<- Overall%>%
  pivot_longer(2:3)






quartiles <- quantile(Overall$HCVPercentPerHouseHold, na.rm =TRUE, probs = seq(0, 1, 1/3))
TotalData <- Overall %>%
  mutate(quartile = ifelse(HCVPercentPerHouseHold>quartiles[3],3,
                           ifelse(HCVPercentPerHouseHold>quartiles[2],2,
                                  1)))

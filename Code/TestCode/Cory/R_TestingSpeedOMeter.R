library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(ggmap)


vars <- load_variables(2020,"acs5/subject")
view(vars)

Data <- read_excel("IowaCostBurden.xlsx") 

DataValue <- read_excel("IowaCostBurdenValue.xlsx")

IowaCounty = read_csv("iowa_county_2010-2016.csv") %>% select(id,fips,county_name)


TotalData <- merge(IowaCounty,Data, by.x = "county_name",by.y = "Area",all.x =TRUE, all.y=FALSE) %>%
  rename(Area = county_name)

TotalData <- TotalData %>%
  group_by(Area)%>%
  mutate(TotalBurden =`Severe Cost Burden` + `Cost Burden`)%>%
  ungroup %>%
  mutate(StateAverage = mean(TotalBurden),CountyMedian = median(TotalBurden))

quartiles <- quantile(TotalData$TotalBurden)

TotalData <- TotalData %>%
  mutate(quartile = ifelse(TotalBurden>quartiles[4],4,
                           ifelse(TotalBurden>quartiles[3],3,
                                  ifelse(TotalBurden>quartiles[2],2,
                                         1))))

TotalData <- merge(TotalData,DataValue, by = "Area")




write.csv(TotalData,"IowaCostBurdenData.csv" )


#selectedCounty = Story County
SelectedCounty <- filter(TotalData, TotalData$Area == "Polk County")




library(plotly)
#https://plotly.com/r/gauge-charts/

fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = SelectedCounty$TotalBurden ,  #value were looking at
  title = list(text = "Total Percent Rental Cost Burden"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference =  SelectedCounty$StateAverage, decreasing = list(color = "green"),increasing = list(color ='red')),
   gauge = list(
    axis = list(range = list(min(TotalData$TotalBurden)-1, max(TotalData$TotalBurden)+1)), #min and max values of graph
    steps = list(
      list(range = c(quartiles[1]-1,quartiles[2]), color = "lightgreen"), #adding value ranges by quartile
      list(range = c(quartiles[2],quartiles[3]), color = "yellow"), #adding value ranges by quartile
      list(range = c(quartiles[3],quartiles[4]), color = "orange"),
      list(range = c(quartiles[4],quartiles[5]+1), color = "red")),
    threshold = list(
      line = list(color = "blue", width = 3),
      thickness = 0.75,
      value = SelectedCounty$StateAverage),
    labels="State Average",
    bar = list(
      color ="black")),
  ids = c(0:7)
    )

fig <- fig %>%
  layout(margin = list(l=30,r=30)) %>%
  add_annotations(
    x= 0.5,
    y=0.2,
    text = "Blue Bar is the State Average",
    showarrow = F
  ) %>%
  add_annotations(
    x= 0.5,
    y=0.52,
    text = SelectedCounty$Area,
    size= 16,
    showarrow = F
  ) %>%
  add_annotations(
    x= 0.04,
    y=0.19,
    text = min(TotalData$TotalBurden),
    size= 16,
    showarrow = F
  )%>%
  add_annotations(
    x= 0.96,
    y=0.19,
    text = max(TotalData$TotalBurden),
    size= 16,
    showarrow = F
  )



fig

#https://github.com/plotly/plotly.js/blob/master/src/components/annotations/attributes.js







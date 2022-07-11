#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(shiny)

library(ggplot2)
library(readxl)
library(tidyverse)
library(readr)
data <- read_csv("OverallDatabase.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test Speedometer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("county", "Select A County", choices = unique(data$county_name)),
            selectInput("indicator", "Select an Indicator", choices = colnames(data)[3:ncol(data)])
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("Plot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Plot <- plotly::renderPlotly({
        # generate bins based on input$bins from ui.R
        Data <- data%>%
          select(county_name,value =input$indicator)
        quartiles <- quantile(Data$value, na.rm =TRUE, probs = seq(0, 1, 1/3))
        TotalData<- Data %>%
          mutate(quartile = ifelse(value>quartiles[3],3,
                            ifelse(value>quartiles[2],2,
                                                       1)))%>%
          mutate(StateAverage = mean(value, na.rm =TRUE),CountyMedian = median(value, na.rm =TRUE))
        SelectedCounty <- filter(TotalData, TotalData$county_name == input$county)
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = SelectedCounty$value ,  #value were looking at
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference =  SelectedCounty$StateAverage, decreasing = list(color = "green"),increasing = list(color ='red')),
          gauge = list(
            axis = list(range = list(min(TotalData$value, na.rm=TRUE), max(TotalData$value, na.rm=TRUE))), #min and max values of graph
            steps = list(
              list(range = c(quartiles[1],quartiles[2]), color = "blue"), #adding value ranges by quartile
              list(range = c(quartiles[2],quartiles[3]), color = "yellow"), #adding value ranges by quartile
              list(range = c(quartiles[3],quartiles[4]), color = "red")),
            threshold = list(
              line = list(color = "brown", width = 3),
              thickness = 0.75,
              value = SelectedCounty$StateAverage),
            labels="State Average",
            bar = list(
              color ="black"))
        )%>%
          layout(margin = list(l=30,r=30)) %>%
          add_annotations(
            x= 0.5,
            y=-0.05,
            text = "Brown Bar is the State Average",
            showarrow = F
          ) %>%
          add_annotations(
            x= 0.5,
            y=0.52,
            text = SelectedCounty$county_name,
            size= 16,
            showarrow = F
          )
    })
}

rsconnect::setAccountInfo(name='coryroth',
                          token='38C64997BF9277F5F54F24924E711BF1',
                          secret='FhJaLpWNXUnHJuMvwz6WibIXV9BtiotSGyCWHZwe')
# Run the application 
shinyApp(ui = ui, server = server)

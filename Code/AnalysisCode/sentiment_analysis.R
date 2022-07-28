library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

### What is the main reason you do not live in Harrison County?
harrison <- read_csv(file.choose())
groups <- iconv(harrison$Q4)
s <- get_nrc_sentiment(groups)
head(s)

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = "main reason you do not live in Harrison County?")


### What community amenities are important to you in choosing a location to live?
groups <- iconv(harrison$Q33)
s <- get_nrc_sentiment(groups)
head(s)

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = "Important community amenities")

### What community amenities are important to you in choosing a location to live?
groups <- iconv(harrison$Q33)
s <- get_nrc_sentiment(groups)
head(s)

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = "Important community amenities")

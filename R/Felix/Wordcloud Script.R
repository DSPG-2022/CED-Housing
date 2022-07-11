library(tidyverse)
library(ggwordcloud)
library(readxl)

words <- read_xlsx("./Data/RawData/WordcloudData/keyword_list.xlsx")

count = c()
for(i in words){
  
  if(words[1, i] ){
    count
  }
}
str_count()
set.seed(42)
ggplot(frequency_dataframe, aes(label = Issue, size = Count, color = factor(sample.int(10, nrow(frequency_dataframe), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  ggtitle("Keyword Wordcloud") +
  scale_size_area(max_size = 24) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#######
install.packages("tidyverse")
install.packages("tidytext")
install.packages("tm")
library(tidyverse)
library(tidytext)
library(tm)

#create stopwords DF
rus_stopwords = data.frame(word = stopwords("ru"))

new_df <- words %>% unnest_tokens(word, Words) 

frequency_dataframe <- new_df %>% 
  count(word) %>% 
  arrange(desc(n)) 

%>%
  head(n=10)
  
### use plotly for interactivity
ggplot(frequency_dataframe, aes(label = word, size = n, color = factor(sample.int(10, nrow(frequency_dataframe), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  ggtitle("Keyword Wordcloud") +
  scale_size_area(max_size = 24) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

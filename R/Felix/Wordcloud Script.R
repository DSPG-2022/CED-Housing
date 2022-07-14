library(tidyverse)
library(ggwordcloud)
library(readxl)
library(plotly)

words <- read_xlsx("./Data/RawData/WordcloudData/keyword_list.xlsx")

frequency_dataframe <- new_df %>% 
  count(word) %>% 
  arrange(desc(n))  %>%
  head(10)
  

cloud <- ggplot(frequency_dataframe, aes(label = word, size = n, color = factor(sample.int(10, nrow(frequency_dataframe), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  ggtitle("Keyword Wordcloud") +
  scale_size_area(max_size = 24) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()


atlantic <- read_csv("./Data/RawData/WordcloudData/AtlanticSurvey.csv")
potta <- read_csv("./Data/RawData/WordcloudData/East PottawattamieSurvey.csv")
harrison <- read_csv("./Data/RawData/WordcloudData/HarrisonSurvey.csv")




######## 
library(udpipe)
library(textrank)
library(lattice)
## First step: Take the English udpipe model and annotate the text. Note: this takes about 3 minutes
atlantic$Q19
atlantic$Q18_5_TEXT
data <- atlantic$Q18_5_TEXT
titl <- "What should be the top priorities for improving housing in Atlantic?"

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = data)
x <- as.data.frame(x)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
option1 <- function(txt, udmodel){
  x <- as.data.frame(udpipe_annotate(ud_model, x = txt))
  stats <- subset(x, upos %in% "NOUN")
  stats <- txt_freq(x = stats$lemma)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  return(stats)
}

stats <- option1(txt = atlantic$Q18_5_TEXT, udmodel = ud_model)
ggplot(stats, aes(label = key, size = freq, color = factor(sample.int(10, nrow(stats), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  ggtitle(titl) +
  scale_size_area(max_size = 24) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

### noun extraction Option 1
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))

atlanticq19 <- ggplot(stats, aes(label = key, size = freq, color = factor(sample.int(10, nrow(stats), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  ggtitle(titl) +
  scale_size_area(max_size = 24) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()
atlanticq19

# Collocation
library(igraph)
library(ggraph)
library(ggplot2)

## Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
head(stats)


wordnetwork <- head(stats, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = titl, subtitle = "Nouns & Adjective")

## option 3
stats <- textrank_keywords(x$lemma, 
                           relevant = x$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 5)
library(wordcloud)
wordcloud(words = stats$keyword, freq = stats$freq)

# Use dependency parsing output to get the nominal subject and the adjective of it
stats <- merge(x, x, 
               by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)
stats <- subset(stats, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
stats$term <- paste(stats$lemma_parent, stats$lemma, sep = " ")
stats <- txt_freq(stats$term)
library(wordcloud)
wordcloud(words = stats$key, freq = stats$freq, min.freq = 3, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))


















data <- tolower(potta$Q33)
titl <- "What community amenities (for example: parks, restaurants, library) are important to you in choosing a location to live?"
model <- udpipe_download_model(language = "english")
model <- udpipe_load_model(model$file_model)
x <- udpipe_annotate(model, x = data)
x <- as.data.frame(x)

## Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
head(stats)


wordnetwork <- head(stats, 50)
wordnetwork <- graph_from_data_frame(wordnetwork)
collocation <- ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = titl, subtitle = "Nouns & Adjective")
collocation

### nouns
stats <- subset(x, upos %in% "NOUN")
stats <- subset(x, upos %in% "ADJ")
stats <- txt_freq(x = stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))

pottaq33 <- ggplot(stats, aes(label = key, size = freq, color = factor(sample.int(10, nrow(stats), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  ggtitle(titl) +
  scale_size_area(max_size = 24) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()
pottaq33


data = atlantic$Q20
titl = atlantic$Q20[1]
x <- udpipe_annotate(model, x = data)
x <- as.data.frame(x)

## Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
head(stats)


wordnetwork <- head(stats, 50)
wordnetwork <- graph_from_data_frame(wordnetwork)


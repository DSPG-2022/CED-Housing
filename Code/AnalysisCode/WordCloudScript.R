# load packages
library(tidyverse)
library(ggwordcloud)
library(readxl)
library(udpipe)
library(textrank)

## First step: Take the English udpipe model and annotate the text. Note: this takes about 3 minutes
# Step must be done for all of the options

# ONLY CHANGE THESE TWO LINES
data = "any string vector that you wish to explore"
titl = "title for graphs"

# Download language model for annotation
ud_model <- udpipe_download_model(language = "english")
# load language model
ud_model <- udpipe_load_model(ud_model$file_model)
# annotate data
x <- as.data.frame(udpipe_annotate(ud_model, x = data))

### Option 1: A simple nouns/adjectives wordcloud ###
# subset annotated data for word type, in this case for nouns
stats <- subset(x, upos %in% "NOUN")
stats <- x %>% filter(upos %in% c("NOUN", "ADJ", "VERB"))
# counting word instances, used for setting word size in the wordcloud
stats <- txt_freq(x = stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))

# this ggplot creates the wordcloud; don't change anything
ggplot(stats, aes(label = key, size = freq, color = factor(sample.int(10, nrow(stats), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  ggtitle(titl) +
  scale_size_area(max_size = 24) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

### Option 2: Collocation and Co-occurrences graph ###
## Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", 
                              group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ", "VERB")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ", "VERB"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ", "VERB"), skipgram = 2)
# verify data
head(stats)

# select top 30 words but can select as many as needed
wordnetwork <- head(stats, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)

# ggraph creates the graph
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = titl, subtitle = "Nouns, Adjective and Verbs")

### data to be used for wordclouds
atlantic <- read_csv("./Data/RawData/WordcloudData/AtlanticSurvey.csv")
potta <- read_csv("./Data/RawData/WordcloudData/East PottawattamieSurvey.csv")
harrison <- read_csv("./Data/RawData/WordcloudData/HarrisonSurvey.csv")

data <- tolower(atlantic$Q18_5_TEXT)
titl <- atlantic$Q18_5_TEXT[1]

data <- tolower(atlantic$Q19)
titl <- atlantic$Q19[1]

data <- tolower(atlantic$Q20)
titl <- atlantic$Q20[1]
  
data <-  tolower(potta$Q33)
titl <- potta$Q33[1]

data <- tolower(harrison$Q33)
titl <-harrison$Q33[1]

data <- tolower(harrison$Q16)
titl <- harrison$Q16[1]















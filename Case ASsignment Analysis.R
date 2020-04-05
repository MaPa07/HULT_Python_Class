# loading all libraries
library(ggplot2)
library(wordcloud)
library(igraph)
library(ggraph)

library(tidyr)
library(tidytext)
library(pdftools) # we need this library to use pdf_text
library(magrittr)
library(rvest)
library(dplyr)
library(textshape)
library(tidyverse)
library(reshape2)
library(tm)
library(textreadr)

library(scales)
library(quanteda)

# Custom Stopwords
custom_stopwords <- tibble(
  word = c(
    "madkudu","velocify","infer","like","NA","01","02","03","04","05","06","07","08","09","10,000","10,001"),
  lexicon = "custom"
)
numeric_stopwords <- tibble(word = seq(1,10000,1), lexicon = "numeric")

# Comining Stop Word Tibbles
stop_words_total <- rbind(stop_words,custom_stopwords,numeric_stopwords)

# Importing all PDF files from the same folder

setwd("/Users/maximilianpaulus/Documents/03_HULT/MBAN_Module B/Text Analytics/Case Assignment")
nm <- list.files(path="/Users/maximilianpaulus/Documents/03_HULT/MBAN_Module B/Text Analytics/Case Assignment")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
infer <- my_pdf_text[1,]
infer_df <- data_frame(text=infer)
infer_df <- infer_df[5:23]
##################################################
# Importing Madkudu PDF
madkudu <- pdf_text("/Users/maximilianpaulus/Documents/03_HULT/MBAN_Module B/Text Analytics/Case Assignment/MadKudu Reviews 2020: Details, Pricing, & Features | G2.pdf") %>%
  readr::read_lines()
madkudu_reviews <- madkudu[53:431]
madkudu_reviews <- madkudu_reviews %>%
  str_squish() 
madkudu_reviews_df <- data_frame(line=1:379, text=madkudu_reviews)
##################################################
# Importing Infer PDF
infer <- pdf_text("/Users/maximilianpaulus/Documents/03_HULT/MBAN_Module B/Text Analytics/Case Assignment/Infer Reviews 2020: Details, Pricing, & Features | G2.pdf") %>%
  readr::read_lines()
infer <- infer %>%
  str_squish() 
infer_df <- data_frame(text=infer)
infer_reviews_df <- infer_df[54:490,]
##################################################
# Importing Velocify PDF
velocify <- pdf_text("/Users/maximilianpaulus/Documents/03_HULT/MBAN_Module B/Text Analytics/Case Assignment/Velocify Lead Manager Reviews 2020: Details, Pricing, & Features | G2.pdf") %>%
  readr::read_lines()
velocify <- velocify %>%
  str_squish() 
velocify_df <- data_frame(text=velocify)
velocify_reviews_df <- infer_df[40:529,]



##################################################
# Removing stopwords, custom lines to remoce and tokenize

linestoremove <- as_tibble(c("What do you like best?","What do you dislike?","Show More","Helpful? Add Comment Share"))

##################################################
# Infer Tokenization and visualization of tokens
infer_tokens <- infer_reviews_df %>%
  anti_join(linestoremove, by = c("text" = "value")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_total) %>%
  count(word, sort=TRUE)

infer_tokens_col <- infer_tokens %>%
  filter(n > 10) %>%
  ggplot(aes(reorder(word,n),n)) + 
  geom_col() + 
  coord_flip()

infer_sentiment <- infer_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words=100, 
                   scale = c(0.5,0.5),
                   fixed.asp = TRUE,
                   title.size = 1)


# Infer bigrams
infer_bigrams_clean <- infer_reviews_df %>%
  anti_join(linestoremove, by = c("text" = "value")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n =2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_total$word) %>%
  filter(!word2 %in% stop_words_total$word) %>%
  count(word1, word2, sort = TRUE)

infer_bigram_graph <- infer_bigrams_clean %>%
  filter(n>5) %>%
  graph_from_data_frame()

ggraph(infer_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##################################################
# Velocify Tokenization and visualization of tokens
velocify_tokens <- velocify_reviews_df %>%
  anti_join(linestoremove, by = c("text" = "value")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_total) %>%
  count(word, sort=TRUE)

velocify_tokens_col <- velocify_tokens %>%
  filter(n > 10) %>%
  ggplot(aes(reorder(word,n),n)) + 
  geom_col() + 
  coord_flip()

velocify_sentiment <- velocify_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words=100, 
                   scale = c(0.5,0.5),
                   fixed.asp = TRUE,
                   title.size = 1)


# Velocify bigrams
velocify_bigrams_clean <- velocify_reviews_df %>%
  anti_join(linestoremove, by = c("text" = "value")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n =2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_total$word) %>%
  filter(!word2 %in% stop_words_total$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  filter(n>2) %>%
  ggplot(aes(reorder(bigram,n),n)) + 
  geom_col() + 
  coord_flip()

velocify_bigram_graph <- velocify_bigrams_clean %>%
  filter(n>5) %>%
  graph_from_data_frame()

ggraph(velocify_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##################################################
# Tokenizing Madkudu df
madkudu_tokens <- madkudu_reviews_df %>%
  anti_join(linestoremove, by = c("text" = "value")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_total) %>%
  count(word, sort=TRUE)

# Madkudu bigrams
madkudu_bigrams_clean <- madkudu_reviews_df %>%
  anti_join(linestoremove, by = c("text" = "value")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n =2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_total$word) %>%
  filter(!word2 %in% stop_words_total$word) %>%
  count(word1, word2, sort = TRUE)
madkudu_bigrams_clean

madkudu_bigram_graph <- madkudu_bigrams_clean %>%
  filter(n>5) %>%
  graph_from_data_frame()

ggraph(infer_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


# Sentiment Analysis
madkudu_sentiment <- madkudu_reviews_df %>%
  anti_join(linestoremove, by = c("text" = "value")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments('nrc')) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words=100, 
                   scale = c(0.5,0.5),
                   fixed.asp = TRUE,
                   title.size = 1
  )


########################################################
# Word Frequency Analysis

frequency <- bind_rows(mutate(madkudu_tokens, company = "MadKudu"), 
                       mutate(velocify_tokens, company = "Velocify"),
                       mutate(infer_tokens, company = "Infer")) %>% 
#  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(company, word) %>% group_by(company) %>% 
  mutate(proportion = n / sum(n)*1000) %>% select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `Infer`:`Velocify`) %>%
  filter(proportion > 1)

# Plotting word frequencies 
#### !!!!!!!!! contains Error !!!!!!!!!!
ggplot(frequency, aes(x = proportion, y = 'MadKudu',
                        color = abs(proportion))) +
                      geom_abline(color = "gray40", lty = 2) +
                      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
                      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
                      #scale_x_log10(labels = percent_format()) +
                      #scale_y_log10(labels = percent_format()) + 
                      scale_color_gradient(limits = c(0, 0.001),
                                           low = "darkslategray4", high = "gray75") + 
                      facet_wrap(~company, ncol = 2) +
                      theme(legend.position="none") + 
                      labs(y = 'MadKudu', x = NULL)

####################################################
#tfidf numerical and graphical

combined_tokens <- bind_rows(
  mutate(madkudu_tokens, company = "MadKudu"),
  mutate(infer_tokens, company = "Infer"),
  mutate(velocify_tokens, company = "Velocify")
)

tokens_tf_idf <- combined_tokens %>%
  count(company, word) %>%
  bind_tf_idf(word, company, n) %>%
  arrange(desc(tf_idf))

tokens_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=2, scales="free")+
  coord_flip()

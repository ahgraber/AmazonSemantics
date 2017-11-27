#-------------------------------------------------------------------------------------------------- 
### Trigrams

library(tidyverse)
library(tidytext)
library(quanteda)
library(ggraph)
library(widyr)

# ngrams and negations https://github.com/kbenoit/quanteda/issues/516
  # requires original df with full review text, not tokens
review_trigrams <- train_df %>%
  unnest_tokens(trigram, Review, token = "ngrams", n = 3)

review_trigrams %>%
  count(trigram, sort = TRUE)

# remove frequently used words from trigrams 
trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

# remove rows containing a stopword in any column
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% custom_spwords$word) %>%
  filter(!word2 %in% custom_spwords$word) %>%
  filter(!word3 %in% custom_spwords$word) 

# new trigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigram_counts

# recombine with original df
trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united

trigram_tf_idf <- trigrams_united %>%
  count(Product, trigram) %>%
  bind_tf_idf(trigram, Product, n) %>%
  arrange(desc(tf_idf))

trigram_tf_idf

# find a tf-idf value where the tf =/= 1 and drop super-high tf-idf terms.
trigram_tf_idf %>%
  arrange(desc(tf_idf))

# look at lowest tf-idf (meaning they appear frequently in reviews)
# *** also consider these for dropping ***
trigram_tf_idf %>%
  arrange(tf_idf)



### look at relationships between trigrams
library(igraph)
library(ggraph)

# original counts
trigram_counts

# filter for only relatively common combinations
trigram_graph <- trigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

trigram_graph

set.seed(2017)
ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# look at relationship with "not"
not_words <- trigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(afinn, by = c(word2 = "word", word3 = "word")) %>%
  count(word2, word3, score, sort = TRUE) %>%
  ungroup()

not_word_graph <- not_words %>%
  filter(n > 5) %>%
  graph_from_data_frame()

ggraph(not_word_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


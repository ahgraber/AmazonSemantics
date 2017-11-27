#-------------------------------------------------------------------------------------------------- 
### Tetragrams

library(tidyverse)
library(tidytext)
library(quanteda)
library(ggraph)
library(widyr)

# ngrams and negations https://github.com/kbenoit/quanteda/issues/516
  # requires original df with full review text, not tokens
review_tetragrams <- train_df %>%
  unnest_tokens(tetragram, Review, token = "ngrams", n = 4)

review_tetragrams %>%
  count(tetragram, sort = TRUE)

# remove frequently used words from tetragrams 
tetragrams_separated <- review_tetragrams %>%
  separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ")

# remove rows containing a stopword in any column
tetragrams_filtered <- tetragrams_separated %>%
  filter(!word1 %in% custom_spwords$word) %>%
  filter(!word2 %in% custom_spwords$word) %>%
  filter(!word3 %in% custom_spwords$word) %>%
  filter(!word4 %in% custom_spwords$word) 

# new tetragram counts:
tetragram_counts <- tetragrams_filtered %>% 
  count(word1, word2, word3, word4, sort = TRUE)

tetragram_counts

### looks like the most popular tetragrams are repeated words - use tetragrams to flag bad reviews?

# recombine with original df
tetragrams_united <- tetragrams_filtered %>%
  unite(tetragram, word1, word2, word3, word4, sep = " ")

tetragrams_united

tetragram_tf_idf <- tetragrams_united %>%
  count(Product, tetragram) %>%
  bind_tf_idf(tetragram, Product, n) %>%
  arrange(desc(tf_idf))

tetragram_tf_idf

# find a tf-idf value where the tf =/= 1 and drop super-high tf-idf terms.
tetragram_tf_idf %>%
  arrange(desc(tf_idf))

# look at lowest tf-idf (meaning they appear frequently in reviews)
# *** also consider these for dropping ***
tetragram_tf_idf %>%
  arrange(tf_idf)



### look at relationships between tetragrams
library(igraph)
library(ggraph)

# original counts
tetragram_counts

# filter for only relatively common combinations
tetragram_graph <- tetragram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

tetragram_graph

set.seed(2017)
ggraph(tetragram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# look at relationship with "not"
not_words <- tetragrams_separated %>%
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


#-------------------------------------------------------------------------------------------------- 
### Bigrams

library(tidyverse)
library(tidytext)
library(quanteda)
library(ggraph)
library(widyr)

# ngrams and negations https://github.com/kbenoit/quanteda/issues/516
  # requires original df with full review text, not tokens
review_bigrams <- train_df %>%
  unnest_tokens(bigram, Review, token = "ngrams", n = 2)

review_bigrams %>%
  count(bigram, sort = TRUE)

# remove frequently used words from bigrams 
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# remove rows containing a stopword in any column
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_spwords$word) %>%
  filter(!word2 %in% custom_spwords$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# recombine with original df
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_tf_idf <- bigrams_united %>%
  count(Product, bigram) %>%
  bind_tf_idf(bigram, Product, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# find a tf-idf value where the tf =/= 1 and drop super-high tf-idf terms.
bigram_tf_idf %>%
  arrange(desc(tf_idf))

# look at lowest tf-idf (meaning they appear frequently in reviews)
# *** also consider these for dropping ***
bigram_tf_idf %>%
  arrange(tf_idf)



### look at relationships between bigrams
library(igraph)
library(ggraph)

# original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 50) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


### Look at correlations between words ---
library(widyr)

# count words co-occuring within review
review_words <- train_df %>%
  unnest_tokens(word, Review)%>%
  filter(!word %in% custom_spwords$word)

review_words

# count words co-occuring within sections
word_pairs <- review_words %>%
  pairwise_count(word, Index, sort = TRUE)

word_pairs

# we need to filter for at least relatively common words first
word_cors <- review_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, Index, sort = TRUE)

word_cors

# map word correlations
set.seed(2016)

word_cors %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# consider using correlation of .2-.4 as cutoff for bigrams we want to keep (?) 
# NOTE: Correlations are NOT necessarily bigrams.  
  # Correlations are simply propensity of words to appear in the same review!
# how can we turn wi fi into a single token?
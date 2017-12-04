#-------------------------------------------------------------------------------------------------- 
### Term-Freq, Inverse-Doc-Freq for proper weighting of frequent/infrequent terms

library(tidyverse)
library(tidytext)

# identify # times word used per App (n / appFreq)
tokens <- train_td %>%
  count(Product, token, sort = TRUE) %>%
  ungroup()

# identify total # words used per App (totalFreq)
total_tokens <- tokens %>% 
  group_by(Product) %>% 
  summarize(totalFreq = sum(n)) %>%
  ungroup()

# create data table with product, word, n, total words
tokens <- left_join(tokens, total_tokens)
# append to train_lem
train_td2 <- left_join(train_td, tokens)
colnames(train_td2) <- c("Index", "Product","Date","Stars","word","appFreq","totalFreq")

# plot term frequency (appFreq/totalFreq) --> we don't have the standard long tail!
ggplot(train_td2, aes(appFreq/totalFreq, fill = Product)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~Product, ncol = 2, scales = "free_y")

# Zipf’s law: the frequency that a word appears is inversely proportional to its rank.
freq_by_rank <- tokens %>% 
  group_by(Product) %>% 
  mutate(rank = row_number(), `term frequency` = appFreq/totalFreq)

freq_by_rank

# plot rank / Product
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Product)) + 
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

# this looks pretty consistent between products (especially between rank 10 and 500(?))
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset) # linear model for central subset

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Product)) + 
  geom_abline(intercept = -0.7754, slope = -1.0323, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

# create tf-idf
tokens <- tokens %>%
  bind_tf_idf(token, Product, n)
  # Term Freq: (# times term appears in a document) / (Total number of terms in the document)
  # Inverse Doc Freq: log_e(Total number of documents / Number of documents with term t in it)
    # IDF is 0 for terms that appear in many documents
  # tf_idf: weight; calculated by tf * idf

# look at highest tf-idf
# theoretically, this means that they are important
tokens %>%
  select(-totalFreq) %>%
  arrange(desc(tf_idf))

# look at lowest tf-idf (meaning they appear frequently in reviews)
# *** also consider these for dropping ***
tokens %>%
  select(-totalFreq) %>%
  arrange(tf_idf)

# based on this, what should we add to stopwords?

rm(tokens)
rm(total_tokens)
rm(freq_by_rank)
rm(rank_subset)
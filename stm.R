install.packages("stm")
install.packages("geometry")
install.packages("Rtsne")
install.packages("rsvd")

library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(geometry)

source("readin.R")

# Import data w/ appropriate col typing
train_lem <- readin(filename="train_lem.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_lem) <- c('Index','Product', 'Date', 'Stars','Review')
train_lem$Product <- as.factor(train_lem$Product)
train_lem$Date <- as.Date(train_lem$Date)

# replace column of reviews with tidy column of tokens
train_td <- train_lem %>%
  unnest_tokens(token, Review) %>%
  filter(!is.na(token))

# remove stop words
## edit stop words as necessary
train_td <- train_td %>%
  filter(!token %in% custom_spwords$word)

# look at frequently used terms per product
train_td %>%
  group_by(Product) %>%
  count(token, sort = TRUE) %>%
  ungroup()

# find location of bottom x%
frequency <- cbind(count(train_td, token, sort=T))
q <- quantile(frequency$n, .80)
  ## words with <= 5 uses account for the lower 80% of our data

# look at infrequently used terms per product
infreqterms <- train_td %>%
  count(token, sort = TRUE) %>%
  filter(n < q) %>%
  ungroup()

# remove infrequent terms
train_td <- anti_join(train_td, infreqterms)

rm(infreqterms)
rm(frequency)

source("monograms.R")
source("bigrams.R")
source("trigrams.R")
source("tetragrams.R")
monograms <- monograms(train_lem)
bigrams <- bigrams(train_lem)
trigrams <- trigrams(train_lem)
tetragrams <- tetragrams(train_lem)

# join monogram, bigram, trigram, tetragram(?)
big_td <- rbind(monograms,bigrams,trigrams,tetragrams)

rm(monograms)
rm(bigrams)
rm(trigrams)
rm(tetragrams)


### code to run topic model
# for n most predictive models, rerurn to explore further
# turn tidy framework into dfm, then add tf_idf, then cast as dtm for topicmodels
train_dfm <- big_td %>%
  select(-Date, -Stars) %>%
  filter(n <= 1000) %>%
  cast_dfm(term = token, document = Index, value = n) %>%
  dfm(remove_punct = TRUE, remove_numbers = TRUE, tolower = TRUE) %>%
  tfidf()


# creating corpus where each review is a distinct document with lemmas
train_lem <- train_lem %>%
  filter(!is.na(Review))

lemma_corpus <- train_lem %>%
  mutate(Index = as.character(Index)) %>%
  corpus(docid_field = "Index", text_field = "Review")

lemma_corpus <- corpus(train_lem, docid_field = NULL, text_field = "Review")

# add potentially relevant additional data back to corpus
docvars(lemma_corpus, "Index") <- train_lem$Index
docvars(lemma_corpus, "Product") <- train_lem$Product
docvars(lemma_corpus, "Date") <- train_lem$Date
docvars(lemma_corpus, "Stars") <- train_lem$Stars

docs <- lemma_corpus$documents 
%>%
  select(index)
vocab <- lemma_corpus$documents 
%>%
  select(Product, Date, Stars)
meta <- lemma_corpus$documents 
%>%
  select(texts)
lemma_corpus$documents <- docs
stm_docs <- prepDocuments(docs, vocab, meta)

# use STM to approximate appropriate # topics
stmModel <- stm(train_dfm, K = 4,
                init.type = "LDA", seed = 1234)

docIndex <- big_td %>%
  select(-Date, -Stars) %>%
  filter(n <= 1000) %>%
  group_by(Index) %>%
  select(Index) %>%
  mutate(dups = duplicated(Index)) %>%
  filter(!dups) %>%
  select(-dups)
docIndex <- left_join(docIndex, train_lem)

findThoughts(stmModel, texts = train_dfm)

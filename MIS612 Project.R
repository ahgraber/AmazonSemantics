# MIS612 Class Project
# Main Script

#--------------------------------------------------------------------------------------------------
### Install packages

# Uncomment and run the first time to ensure all packages are installed

###* note "##" means we did not end up using this package

# install.packages("tidyverse")
# install.packages("textclean")
# install.packages("quanteda")
# install.packages("textstem")
# install.packages("tidytext")
# install.packages("ggraph")
# install.packages("widyr")
# install.packages("topicmodels")


# install.packages("devtools")
# devtools::install_github("ahgraber/tmt")

# install.packages("stm")
# install.packages("hunspell")
# install.packages("NLP")

#
## install.packages("reshape2") 
## install.packages("tm")
## install.packages("qdap")
## install.packages('RQDA')

## install.packages("RSentiment") 
  # see: https://cran.r-project.org/web/packages/RSentiment/vignettes/Introduction.html
## install.packages("SentimentAnalysis") 
  # see: https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
## install.packages("sentiment") # unavailable for R 3.4.1

#--------------------------------------------------------------------------------------------------
### Collect Data
source("scrapeAmazon.R")

# What to scrape?  *** Scraping steaming music apps - NOTE: AMAZON/ANDROID ONLY ***

# # Amazon: https://www.amazon.com/product-reviews/B004FRX0MY/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# amazon_df <- scrapeAmazon("Amazon", "B004FRX0MY", 1379, delay=2)
# # Save data 
# write.csv(amazon_df, "Amazon.csv", row.names=F)
# 
# # iHeartRadio: https://www.amazon.com/product-reviews/B005ZFOOE8/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# iheartradio_df <- scrapeAmazon("iHeartRadio", "B005ZFOOE8", 1384, delay=2)
# # Save data 
# write.csv(iheartradio_df, "iHeartRadio.csv", row.names=F)
# 
# # Pandora: https://www.amazon.com/product-reviews/B005V1N71W/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# pandora_df <- scrapeAmazon("Pandora", "B005V1N71W", 1979, delay=2)
# # Save data 
# write.csv(pandora_df, "Pandora.csv", row.names=F)
# 
# # Spotify: https://www.amazon.com/product-reviews/B00KLBR6IC/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# spotify_df <- scrapeAmazon("Spotify", "B00KLBR6IC", 1504, delay=2)
# # Save data 
# write.csv(spotify_df, "Spotify.csv", row.names=F)

# # Aggregate all data into master file
# data_df <- rbind(amazon_df, iheartradio_df, pandora_df, spotify_df)
# write.csv(data_df, file.path(paste(getwd(),"Scraped Data",sep="/"),"data.csv", row.names=F))

# remove script from environment
rm('scrapeAmazon')
#--------------------------------------------------------------------------------------------------
### Raw Data Cleaning

library(tidyverse)

# Initialize scripts
source("readin.R")
source("firstClean.R")
source("findTypos.R")
source("fixTypos.R")

# Import data w/ appropriate col typing
data_df <- readin(filename="data.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(data_df) <- c('Index','Product', 'Title', 'Author', 'Date', 'Stars', 'Review')

# create data frame of stuff not-to-clean (dates, stars, etc)
preserve_df <- as_data_frame(cbind(data_df$Product, data_df$Date, data_df$Stars))  ## NOTE: DROPS AUTHOR!!!
colnames(preserve_df) <- c('Product', 'Date', 'Stars')
preserve_df$Product <- as.factor(preserve_df$Product)
preserve_df$Date <- as.Date(preserve_df$Date, format="%B%d,%Y")

# Combine title & review text to be cleaned
toclean_df <- as_data_frame(paste(data_df$Title," ",data_df$Review))

### typo find/replace does not work currently
# # identify typos in data
# typolist <- findTypos(toclean_df)
# 
# # clean reviews and app
# cleaned1 <- firstClean(toclean_df)
# # use to find and replace common typos
# cleaned2 <- fixTypos(cleaned1)
# # run additional cleaning
# cleaned_df <- firstClean(cleaned2)
# 
# # check to see if typos removed
# typolist2 <- findTypos(cleaned_df)

# remove once typo replacement is fixed
cleaned_df <- firstClean(toclean_df)

# combine reviews and formatted other stuff
c_data_df <- bind_cols(preserve_df,cleaned_df)
colnames(c_data_df) <- c('Product', 'Date', 'Stars','Review')
c_data_df$Product <- as.factor(c_data_df$Product)
c_data_df$Date <- as.Date(c_data_df$Date, format="%B%d,%Y")

# save cleaned data
write.csv(c_data_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"c_data.csv"))

# remove working files from environment
rm(data_df)
rm(preserve_df)
rm(toclean_df)
rm(cleaned1)
rm(cleaned2)
rm(cleaned_df)
rm('firstClean')
#--------------------------------------------------------------------------------------------------
### Create Training Set, Testing Set

library(tidyverse)

# Initialize scripts
source("readin.R")
source("randomSample.R")

# Import data w/ appropriate col typing
c_data_df <- readin(filename="c_data.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(c_data_df) <- c('Index','Product', 'Date', 'Stars','Review')
c_data_df$Product <- as.factor(c_data_df$Product)
c_data_df$Date <- as.Date(c_data_df$Date)

# Randomly select 30 percent of the data and store it in a data frame
booleans <- data_frame(randomSample(c_data_df, 30))
colnames(booleans) <- 'booleans'
sample <- bind_cols(c_data_df,booleans)

# create training and testing set
train_df <- filter(sample, sample$booleans)
test_df <- filter(sample, !sample$booleans)

# remove booleans selectors from training/testing set
train_df[ncol(train_df)] <- NULL
test_df[ncol(test_df)] <- NULL

# save training subset
write.csv(train_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"train.csv"), row.names = F)
write.csv(test_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"test.csv"), row.names = F)

# remove working data
rm(c_data_df)
rm(booleans)
rm(sample)
#--------------------------------------------------------------------------------------------------
### Read in training set

library(tidyverse)

# Initialize scripts
source("readin.R")

# Import data w/ appropriate col typing
train_df <- readin(filename="train.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_df) <- c('Index','Product', 'Date', 'Stars','Review')
train_df$Product <- as.factor(train_df$Product)
train_df$Date <- as.Date(train_df$Date)

#-------------------------------------------------------------------------------------------------- 
### Create corpora

library(tidyverse)
library(quanteda)
library(textstem)

# creating corpus where each review is a distinct document
review_corpus <- corpus(train_df$Review)
# add potentially relevant additional data back to corpus
docvars(review_corpus, "Index") <- train_df$Index
docvars(review_corpus, "Product") <- train_df$Product
docvars(review_corpus, "Date") <- train_df$Date
docvars(review_corpus, "Stars") <- train_df$Stars

summary(review_corpus)

# creating corpus where each review is a distinct document with lemmas
lemma_corpus <- corpus(lemmatize_strings(train_df$Review,dictionary = lexicon::hash_lemmas))
# add potentially relevant additional data back to corpus
docvars(lemma_corpus, "Index") <- train_df$Index
docvars(lemma_corpus, "Product") <- train_df$Product
docvars(lemma_corpus, "Date") <- train_df$Date
docvars(lemma_corpus, "Stars") <- train_df$Stars

summary(lemma_corpus)

# rm(review_corpus)
# rm(lemma_corpus)

#-------------------------------------------------------------------------------------------------- 
### Stopword management - see wordListMgmt.R

# import revised stop word list
custom_spwords <- readin(filename="custom_spwords.txt", subfolder="Lists", infolder=T)

#-------------------------------------------------------------------------------------------------- 
### Synonym management (grammantical)

# see wordListMgmt.R 

#-------------------------------------------------------------------------------------------------- 
### Tidy Tokens

library(tidyverse)
library(tidytext)
library(textstem)
library(quanteda)

train_lem <- train_df
train_lem$Review <- lemmatize_strings(train_df$Review, dictionary = lexicon::hash_lemmas)

# replace column of reviews with tidy column of tokens
train_lem <- train_lem %>%
  unnest_tokens(word, Review)

# replace typos(?)

# remove stop words
## edit stop words as necessary
train_lem <- train_lem %>%
  filter(!word %in% custom_spwords$word)

# look at frequently used terms per product
train_lem %>%
  group_by(Product) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# plot frequently used words
train_lem %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
  
# compare frequent words for different apps
frequency <- train_lem %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%   # redundant if tokens well-cleaned
  count(Product, word) %>%
  group_by(Product) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Product, proportion) %>% 
  gather(Product, proportion, `iHeartRadio`:`Spotify`)

library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Amazon`, color = abs(`Amazon` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~Product, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Amazon", x = NULL)

rm(frequency)
#-------------------------------------------------------------------------------------------------- 
### Term-Freq, Inverse-Doc-Freq for proper weighting of frequent/infrequent terms

library(tidyverse)
library(tidytext)

# identify # times word used per App (term-freq)
count_word <- train_lem %>%
  count(Product, word, sort = TRUE) %>%
  ungroup()

# identify total # words used per App (for inverse-doc freq)
total_words <- count_word %>% 
  group_by(Product) %>% 
  summarize(total = sum(n)) %>%
  ungroup()

# create data table with product, word, n, total words
count_word <- left_join(count_word, total_words)
# append to train_lem
train_lem2 <- left_join(train_lem, count_word)

ggplot(train_lem2, aes(n/total, fill = Product)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~Product, ncol = 2, scales = "free_y")

# create tf-idf
count_word <- count_word %>%
  bind_tf_idf(word, Product, n)

# look at highest tf-idf (meaning infrequently used)
# theoretically, this means that they are important - but for us, 
# it seems like most are single-use gibberish or one-off typos
  # *** should consider these for dropping ***
# find a tf-idf value where the tf =/= 1 and drop super-high tf-idf terms.
count_word %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# look at lowest tf-idf (meaning they appear frequently in reviews)
# *** also consider these for dropping ***
count_word %>%
  select(-total) %>%
  arrange(tf_idf)

# based on this, what should we add to stopwords?

rm(count_word)
rm(total_words)
rm(train_lem2)

#-------------------------------------------------------------------------------------------------- 
### n-grams

# see bigrams.R
# see trigrams.R
# see tetragrams.R

#--------------------------------------------------------------------------------------------------
### Sentiment analysis with tidytext

library(tidyverse)
library(tidytext)
library(quanteda)

# get sentiments 
bing <- get_sentiments("bing")  # positive/negative binary
nrc <- get_sentiments("nrc")    # with emotion
afinn <- get_sentiments("afinn")    # positive/negative score

# or read in from edited files
bing <- readin("bing_sentiment.txt", subfolder = "Lists",infolder = T)
nrc <- readin("nrc_sentiment.txt", subfolder = "Lists",infolder = T)
afinn <- readin("afinn_sentiment.txt", subfolder = "Lists",infolder = T)


# train_lem is our tidy structure of words
# join +/- sentiment from "bing"
# count
# spread positive / negative score into separate columns
# calculate sentiment delta
train_bing <- train_lem %>%
  inner_join(bing) %>%
  count(Product, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method = "Bing")

train_nrc <- train_lem %>%
  inner_join(nrc) %>%
  count(Product, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method = "NRC")

train_afinn <- train_lem %>%
  inner_join(afinn) %>% 
  group_by(Product) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

# comparison plot of all 3 sentiments
bind_rows(train_bing, train_nrc, train_afinn) %>%
  ggplot(aes(Product, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


# see 4.1.3 Using bigrams to provide context in sentiment analysis here: 
  #http://tidytextmining.com/ngrams.html
# must have bigram lists
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

# chart relationship based on afinn-score of associated word
not_word_graph <- not_words %>%
  filter(n > 3) %>%
  graph_from_data_frame()

ggraph(not_word_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# look at contribution of second word
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

# look at how negation words impact subsequent term in bigram
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() +
  facet_wrap(~word1, ncol = 2, scales = "free_y")


### how do we want to use the negation to improve our sentiments?

#-------------------------------------------------------------------------------------------------- 
### Document-Term Matrix creation, cleaning, simplification leading into models
  # if needed, see section 5 here: http://tidytextmining.com/dtm.html

library(quanteda)
library(textstem)

# could create DTM and then tidy from here

# Create Doc-Term Matrix from corpus; w/ lemma replacement and stemming(?), and stopwords removed
docMatrix <- dfm(lemma_corpus,
                 remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE,
                 remove = custom_spwords$word, stem = TRUE)

# Top 50 words
topfeatures(docMatrix, 50)


# create Doc-Term Matrix grouping by App
docMatrixApp <- dfm(lemma_corpus, ## lembag vs textbag
                     groups = "Product",
                     remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE,
                     remove = custom_spwords$word, stem = TRUE)

# create Doc-Term Matrix grouping by Star Rating
docMatrixStar <- dfm(lemma_corpus, ## lembag vs textbag
                     groups = "Stars",
                     remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE,
                     remove = custom_spwords$word, stem = TRUE)


#--------------------------------------------------------------------------------------------------

### Themes & Topic models with TM
library(topicmodels)

# create td-idf by review index
train_lem3 <- train_lem %>%
  count(Index, word) %>%
  bind_tf_idf(word, Index, n) %>%
  arrange(desc(tf_idf))

# cast into dtm for topic model
train_dtm <- cast_dtm(train_lem3, Index, word, n)

# set a seed so that the output of the model is predictable
total_lda <- LDA(train_dtm, k = 2, control = list(seed = 1234))
  ## A LDA_VEM topic model with 2 topics.

# extracting the per-topic-per-word probabilities, called β (“beta”), from the model
total_topics <- tidy(total_lda, matrix = "beta")
total_topics

# find top 10 terms
total_top_terms <- total_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# graph top 10 terms per topic (terms are damn near equivalent???)
  # try again filtering by App
total_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# identify words that have greatest difference between topics
beta_spread <- total_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

top_beta_spread <- beta_spread %>%
  top_n(20, abs(log_ratio)) %>%
  ungroup() %>%
  arrange(term, -log_ratio)

top_beta_spread %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = factor(term))) +
  geom_col(show.legend = FALSE) +
  coord_flip()
  
#examine the per-document-per-topic probabilities, called γ (“gamma”)
total_documents <- tidy(total_lda, matrix = "gamma")
total_documents





#--------------------------------------------------------------------------------------------------
### Themes & Topic models with STM

# https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html#corpus-management-tools 
library(stm) # or topicmodels (using Latent Dirichlet allocation)

# plotRemoved will plot the number of words and documents removed for different thresholds
plotRemoved(docMatrix$Re, lower.thresh = seq(1, 200, by = 100))

out <- prepDocuments(processed$documents, processed$vocab, 
                     processed$meta, lower.thresh = 15)





### Build dictionary for hypothesized themes
# myDict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
#                           economy = c("jobs", "business", "grow", "work")))

# byPresMat <- dfm(recentCorpus, dictionary = myDict)
# byPresMat


# quantdfm <- dfm(data_corpus_irishbudget2010, 
#                 remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("english"))
# quantdfm <- dfm_trim(quantdfm, min_count = 4, max_docfreq = 10, verbose = TRUE)
# ## Removing features occurring:
# ##   - fewer than 4 times: 3,527
# ##   - in more than 10 documents: 72
# ##   Total features removed: 3,599 (73.8%).
# quantdfm
# ## Document-feature matrix of: 14 documents, 1,279 features (64.6% sparse).
# 
# if (require(topicmodels)) {
#   myLDAfit20 <- LDA(convert(quantdfm, to = "topicmodels"), k = 20)
#   get_terms(myLDAfit20, 5)
# }

#--------------------------------------------------------------------------------------------------

# Similarities between apps or star ratings

# https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html#corpus-management-tools 

# presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
#                remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
# obamaSimil <- textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), 
#                              margin = "documents", method = "cosine")

# data(data_corpus_SOTU, package = "quantedaData")
# presDfm <- dfm(corpus_subset(data_corpus_SOTU, Date > as.Date("1980-01-01")), 
#                stem = TRUE, remove_punct = TRUE,
#                remove = stopwords("english"))
# presDfm <- dfm_trim(presDfm, min_count = 5, min_docfreq = 3)
# # hierarchical clustering - get distances on normalized dfm
# presDistMat <- textstat_dist(dfm_weight(presDfm, "relfreq"))
# # hiarchical clustering the distance object
# presCluster <- hclust(presDistMat)
# # label with document names
# presCluster$labels <- docnames(presDfm)
# # plot as a dendrogram
# plot(presCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")


#--------------------------------------------------------------------------------------------------
### Standard EOF
# Detach data from search path!
detach() # Repeat as necessary

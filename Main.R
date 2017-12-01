# MIS612 Class Project
# Main Script

#--------------------------------------------------------------------------------------------------
### Install packages

# Uncomment and run the first time to ensure all packages are installed

###* note "##" means we did not end up using this package

# install.packages("tidyverse")
# install.packages("stringr")
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

# Import data w/ appropriate col typing
data_df <- readin(filename="data.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(data_df) <- c('Index','Product', 'Title', 'Author', 'Date', 'Stars', 'Review')

# create data frame of stuff not-to-clean (dates, stars, etc)
preserve_df <- as_data_frame(cbind(data_df$Product, data_df$Date, data_df$Stars))  ## NOTE: DROPS AUTHOR!!!
colnames(preserve_df) <- c('Product', 'Date', 'Stars')
preserve_df$Product <- as.factor(preserve_df$Product)
preserve_df$Date <- as.Date(preserve_df$Date, format="%B%d,%Y")

# Combine title & review text to be cleaned
review_df <- as_data_frame(paste(data_df$Title," ",data_df$Review))

# combine reviews and formatted other stuff
c_data_df <- bind_cols(preserve_df,review_df)
colnames(c_data_df) <- c('Product', 'Date', 'Stars','Review')
c_data_df$Product <- as.factor(c_data_df$Product)
c_data_df$Date <- as.Date(c_data_df$Date, format="%B%d,%Y")

# save cleaned data
write.csv(c_data_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"c_data.csv"))

# remove working files from environment
rm(data_df)
rm(preserve_df)
rm(review_df)

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

# save subsets
write.csv(train_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"train.csv"), row.names = F)
write.csv(test_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"test.csv"), row.names = F)

# remove working data
rm(c_data_df)
rm(booleans)
rm(sample)
rm(randomSample)
rm(test_df)
#--------------------------------------------------------------------------------------------------
### Clean training set

library(tidyverse)
library(textclean)
library(textstem)

# Initialize scripts
source("readin.R")
source("firstClean.R")
source("findTypos.R")
source("fixTypos.R")

# Import data w/ appropriate col typing
train_df <- readin(filename="train.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_df) <- c('Index','Product', 'Date', 'Stars','Review')
train_df$Product <- as.factor(train_df$Product)
train_df$Date <- as.Date(train_df$Date)

train_lem <- train_df
toclean <- train_df$Review

# identify typos in data
typolist <- findTypos(toclean)

# clean reviews and app
  # replace_number seems to be causing problems
cleaned1 <- firstClean(toclean)
# use to find and replace common typos
cleaned2 <- fixTypos(cleaned1)

# check to see if typos removed
typolist2 <- findTypos(cleaned2)

# lemmatize
cleaned3 <- lemmatize_strings(cleaned2, dictionary = lexicon::hash_lemmas)

# remove once typo replacement is fixed
train_df$Review <- cleaned2
colnames(train_df) <- c('Index','Product', 'Date', 'Stars','Review')

train_lem$Review <- cleaned3
colnames(train_lem) <- c('Index','Product', 'Date', 'Stars','Review')

# save the cleaned file!
write.csv(train_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"train.csv"), row.names = F)
write.csv(train_lem, file.path(paste(getwd(),"Scraped Data",sep = "/"),"train_lem.csv"), row.names = F)

# remove working data
rm(toclean)
rm(cleaned1)
rm(cleaned2)
rm(cleaned3)
rm(typolist)
rm(typolist2)
rm(firstClean)
rm(findTypos)
rm(fixTypos)

#-------------------------------------------------------------------------------------------------- 
### Create corpora

library(tidyverse)
library(quanteda)
source("readin.R")

# Import data w/ appropriate col typing
train_df <- readin(filename="train.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_df) <- c('Index','Product', 'Date', 'Stars','Review')
train_df$Product <- as.factor(train_df$Product)
train_df$Date <- as.Date(train_df$Date)

# creating corpus where each review is a distinct document with lemmas
review_corpus <- corpus(train_df$Review)
# add potentially relevant additional data back to corpus
docvars(review_corpus, "Index") <- train_df$Index
docvars(review_corpus, "Product") <- train_df$Product
docvars(review_corpus, "Date") <- train_df$Date
docvars(review_corpus, "Stars") <- train_df$Stars

summary(review_corpus)

# Import data w/ appropriate col typing
train_lem <- readin(filename="train_lem.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_lem) <- c('Index','Product', 'Date', 'Stars','Review')
train_lem$Product <- as.factor(train_lem$Product)
train_lem$Date <- as.Date(train_lem$Date)

# creating corpus where each review is a distinct document with lemmas
lemma_corpus <- corpus(train_lem$Review)
# add potentially relevant additional data back to corpus
docvars(lemma_corpus, "Index") <- train_lem$Index
docvars(lemma_corpus, "Product") <- train_lem$Product
docvars(lemma_corpus, "Date") <- train_lem$Date
docvars(lemma_corpus, "Stars") <- train_lem$Stars

summary(lemma_corpus)

# rm(review_corpus)
# rm(lemma_corpus)

#-------------------------------------------------------------------------------------------------- 
### Stopword management - see wordListMgmt.R

# import revised stop word list
custom_spwords <- readin(filename="custom_spwords.csv", subfolder="Lists", infolder=T)

#-------------------------------------------------------------------------------------------------- 
### Tidy Tokens

library(tidyverse)
library(tidytext)
library(quanteda)
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
quantile(frequency$n, .80)
  ## words with <= 5 uses account for the lower 80% of our data

# look at infrequently used terms per product
infreqterms <- train_td %>%
  count(token, sort = TRUE) %>%
  filter(n<5) %>%
  ungroup()

# remove infrequent terms
train_td <- anti_join(train_td, infreqterms)

### exploration ###
# plot frequently used words
train_td %>%
  count(token, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(token = reorder(token, n)) %>%
  ggplot(aes(token, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# compare frequent words for different apps
frequency <- train_td %>%
  mutate(token = str_extract(token, "[a-z']+")) %>%   # redundant if tokens well-cleaned
  count(Product, token) %>%
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

rm(infreqterms)
rm(frequency)

#-------------------------------------------------------------------------------------------------- 
### n-grams

source("bigrams.R")
source("trigrams.R")
source("tetragrams.R")
bigrams <- bigrams(train_lem)
trigrams <- trigrams(train_lem)
tetragrams <- tetragrams(train_lem)

# add frequency to train_td
monograms <- train_td %>%
  add_count(token)

# join monogram, bigram, trigram, tetragram(?)
big_td <- rbind(monograms,bigrams,trigrams,tetragrams)


#-------------------------------------------------------------------------------------------------- 
### Identify relationship between token and star rating
source("tokenValues.R")

token_Stars <- tokenValues(big_td, "Index")

# review distributions
ggplot(token_Stars, aes(avg_rating)) + geom_histogram()
ggplot(token_Stars, aes(avg_stdev)) + geom_histogram()

# save word x star data
write.csv(token_Stars, file.path(paste(getwd(),"Lists",sep = "/"),"token_Stars.csv"))


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
bing <- readin("bing_sentiment.csv", subfolder = "Lists",infolder = T)
nrc <- readin("nrc_sentiment.csv", subfolder = "Lists",infolder = T)
afinn <- readin("afinn_sentiment.csv", subfolder = "Lists",infolder = T)


# train_td is our tidy structure of words
# join +/- sentiment from "bing"
# count
# spread positive / negative score into separate columns
# calculate sentiment delta
train_bing <- train_td %>%
  inner_join(bing) %>%
  count(Product, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method = "Bing")

train_nrc <- train_td %>%
  inner_join(nrc) %>%
  count(Product, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method = "NRC")

train_afinn <- train_td %>%
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
source("topicgraph.R")
topicnumber = 9          #edit this for number of topicmodels/topic graphs
topicgraph(train_td, topicnumber)

# create td-idf by review index
train_td3 <- train_td %>%
  count(Index, word) %>%
  bind_tf_idf(word, Index, n) %>%
  arrange(desc(tf_idf))

# cast into dtm for topic model
train_dtm <- cast_dtm(train_td3, Index, word, n)

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

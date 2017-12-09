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
# install.packages("zoo")

## install.packages("reshape2") 
## install.packages("tm")

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
# typolist <- findTypos(toclean)

# clean reviews and app
  # replace_number seems to be causing problems
cleaned1 <- firstClean(toclean)
# use to find and replace common typos
cleaned2 <- fixTypos(cleaned1)

# check to see if typos removed
# typolist2 <- findTypos(cleaned2)

# lemmatize
cleaned3 <- lemmatize_strings(cleaned2, dictionary = lexicon::hash_lemmas)

# use the cleaned, lemmatized data
train_lem$Review <- cleaned3 %>%
  filter(!is.na(Review))
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
q <- quantile(frequency$n, .80)
  ## words with <= 5 uses account for the lower 80% of our data

# look at infrequently used terms per product
infreqterms <- train_td %>%
  count(token, sort = TRUE) %>%
  filter(n < q) %>%
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
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
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

# save big_td
write.csv(big_td, file.path(paste(getwd(),"Lists",sep = "/"),"big_td.csv"), row.names=F)

rm(monograms)
rm(bigrams)
rm(trigrams)
rm(tetragrams)

#-------------------------------------------------------------------------------------------------- 
### Identify relationship between token and star rating
source("tokenValues.R")
source("readin.R")

# Import data w/ appropriate col typing
big_td <- readin(filename="big_td.csv", subfolder="Lists", infolder=TRUE)
colnames(big_td) <- c('Index','Product', 'Date', 'Stars','token','n')
big_td$Product <- as.factor(big_td$Product)
big_td$Date <- as.Date(big_td$Date)

token_Stars <- tokenValues(big_td, "Index")

# review distributions
ggplot(token_Stars, aes(avg_rating)) + geom_histogram()
ggplot(token_Stars, aes(avg_stdev)) + geom_histogram()
ggplot(token_Stars, aes(tf)) + geom_histogram()
ggplot(token_Stars, aes(idf)) + geom_histogram()
ggplot(token_Stars, aes(avg_err)) + geom_histogram()

# check relationship between actual and average rating for each token
cor(token_Stars$Stars, token_Stars$avg_rating)

# visually confirm relationship
ggplot(token_Stars, aes(Stars, avg_err)) +
  geom_jitter(position = position_jitter(width = .1),
              alpha = .05)

### looks like we could -0.5 from 1,2,3-star tokens, and +0.5 to 5-star tokens...
  # would that kind of linear error adjustment fall out of the linear model?

# save word x star data
write.csv(token_Stars, file.path(paste(getwd(),"Lists",sep = "/"),"token_Stars.csv"), row.names=F)

token_Stars <- readin(filename="token_Stars.csv", subfolder="Lists", infolder=TRUE)
#--------------------------------------------------------------------------------------------------
### Create Sentiment model

# see predictiveModel.R for star-rating prediciton
# see predictiveModelBinary.R for positive/negative prediction

#--------------------------------------------------------------------------------------------------
### Time series analysis

library(zoo)

source("readin.R")

# Import data w/ appropriate col typing
train_lem <- readin(filename="train_lem.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_lem) <- c('Index','Product', 'Date', 'Stars','Review')
train_lem$Product <- as.factor(train_lem$Product)
train_lem$Date <- as.Date(train_lem$Date)

# Create aggregated data by quarter
time_series <- train_lem %>%
  mutate(sentiment = case_when(
    Stars >= 4 ~ 1,          # 4,5 = positive
    Stars < 4 ~ 0)) %>%      # 1-3 = negative
  ungroup()

time_series <- time_series %>%
  mutate(quarter = as.Date(as.yearqtr(time_series$Date))) %>%
  group_by(quarter, Product) %>%
  summarise(avg_sentiment = mean(sentiment))

# create prediction data
time_series_prediction <- left_join(review_aggregate_test, test_lem)

time_series_prediction <- time_series_prediction %>%
  mutate(prediction= predict(model2, newdata = data_frame(score), type="response")) %>%
  mutate(int_prediction = round(prediction, 0))

time_series_prediction <- time_series_prediction %>%  
  mutate(quarter = as.Date(as.yearqtr(time_series_prediction$Date))) %>%
  group_by(quarter, Product) %>%
  summarise(avg_prediction = mean(int_prediction))

# filter time series by product
amazon_time_series <- time_series %>%
  filter(Product == "Amazon")
spotify_time_series <- time_series %>%
  filter(Product == "Spotify")
pandora_time_series <- time_series %>%
  filter(Product == "Pandora")
iheartradio_time_series <- time_series %>%
  filter(Product == "iHeartRadio")

amazon_time_series_prediction <- time_series_prediction %>%
  filter(Product == "Amazon")
spotify_time_series_prediction <- time_series_prediction %>%
  filter(Product == "Spotify")
pandora_time_series_prediction <- time_series_prediction %>%
  filter(Product == "Pandora")
iheartradio_time_series_prediction <- time_series_prediction %>%
  filter(Product == "iHeartRadio")

# ones vectors for area charts
ones_amazon <- seq(1,1,length.out=length(amazon_time_series$quarter))
ones_spotify <- seq(1,1,length.out=length(spotify_time_series$quarter))
ones_pandora <- seq(1,1,length.out=length(pandora_time_series$quarter))
ones_iheartradio <- seq(1,1,length.out=length(iheartradio_time_series$quarter))

# charts
ggplot(amazon_time_series) + 
  geom_area(aes(quarter, ones_amazon), fill = "#5ea34d") + 
  geom_area(aes(quarter, 1-avg_sentiment), fill = "#b54747") + 
  geom_line(aes(quarter, 1-amazon_time_series_prediction$avg_prediction), 
            colour = "black", size = 2)
ggplot(spotify_time_series) + 
  geom_area(aes(quarter, ones_spotify), fill = "#5ea34d") + 
  geom_area(aes(quarter, 1-avg_sentiment), fill = "#b54747") + 
  geom_line(aes(quarter, 1-spotify_time_series_prediction$avg_prediction), 
            colour = "black", size = 2)
ggplot(pandora_time_series) + 
  geom_area(aes(quarter, ones_pandora), fill = "#5ea34d") + 
  geom_area(aes(quarter, 1-avg_sentiment), fill = "#b54747") + 
  geom_line(aes(quarter, 1-pandora_time_series_prediction$avg_prediction), 
            colour = "black", size = 2)
ggplot(iheartradio_time_series) + 
  geom_area(aes(quarter, ones_iheartradio), fill = "#5ea34d") + 
  geom_area(aes(quarter, 1-avg_sentiment), fill = "#b54747") + 
  geom_line(aes(quarter, 1-iheartradio_time_series_prediction$avg_prediction), 
            colour = "black", size = 2)

#--------------------------------------------------------------------------------------------------

### Sentiment analysis with tidytext notes
  # to do this, we may want to start doing the dictionary creation within the ngrams step
  # i.e., earlier in the process
    # or
  # could run separated on big_td, but be prepared for NAs when not tetragrams
# separated <- big_td %>%
#   separate(token, c("word1", "word2", "word3", "word4"), sep = " ")


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
### Create corpora

library(tidyverse)
library(quanteda)
library(textstem)
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

# look at keywords in context as needed
options(width = 200)
kwic(corpus_subset(lemma_corpus, Product == "iHeartRadio"), "Pandora")

# rm(review_corpus)
# rm(lemma_corpus)

#--------------------------------------------------------------------------------------------------
### Themes & Topic models with TM

library(zoo)

source("topicgraph.R")
source("readin.R")

# Import data w/ appropriate col typing
big_td <- readin(filename="big_td.csv", subfolder="Lists", infolder=TRUE)
colnames(big_td) <- c('Index','Product', 'Date', 'Stars','token','n')
big_td$Product <- as.factor(big_td$Product)
big_td$Date <- as.Date(big_td$Date)

# create per-app datasets
amazon_td <- big_td %>%
  filter(Product == 'Amazon')

amazon_pos <- amazon_td %>%
  filter(Stars >= 4)

amazon_neg <- amazon_td %>%
  filter(Stars < 4)

ihr_td <- big_td %>%
  filter(Product == 'iHeartRadio')
pandora_td <- big_td %>%
  filter(Product == 'Pandora')
spotify_td <- big_td %>%
  filter(Product == 'Spotify')

### series to review top terms in n models
# edit this for number of topicmodels/topic graphs
topicnumber = 4

amazonpos <- topicgraph(amazon_pos, topicnumber)
amazonpos$plots
amazonpos.topics<-as_data_frame(amazonpos$topics) %>%
  mutate(amazonTopic = topic) %>%
  select(-topic)
# 1 = amazon device friendly
# 2 = amazon prime
# 3 = better than pandora
# 4 = free & easy

      # negative topics don't make sense :(
      amazonneg <- topicgraph(amazon_neg, 9)
      amazonneg$plots
      amazonneg.topics<-as_data_frame(amazonneg$topics) %>%
        mutate(amazonTopic = topic) %>%
        select(-topic)

      
### code to run topic model
# for n most predictive models, rerurn to explore further
# turn tidy framework into dfm, then add tf_idf, then cast as dtm for topicmodels
amazon_dfm <- amazon_pos %>%
  cast_dfm(term = token, document = Index, value = n) %>%
  dfm(remove_punct = TRUE, remove_numbers = TRUE, tolower = TRUE) %>%
  dfm_trim(min_count = 4, max_docfreq = 1000, verbose = TRUE) %>%
  tfidf() 

if (require(topicmodels)) {
    amazonPosLDA <- LDA(convert(amazon_dfm, to = "topicmodels"), k = topicnumber, 
                    control = list(seed = 1234))
    ihr_terms <- get_terms(amazonPosLDA, 20)
}

# examine the per-document-per-topic probabilities, called γ (“gamma”)
# this is essentially a classificiation model!
gammas <- tidy(amazonPosLDA, matrix = "gamma")
gammas$document <- as.integer(gammas$document)

# turn each topic into a column
gammas <- gammas %>%
  spread(topic, gamma)
colnames(gammas) <- c("Index", "Gamma1","Gamma2","Gamma3","Gamma4")
# recode whether the review contains the topic
topicGammas <- gammas %>%
  mutate(Topic1 = if_else(Gamma1 > 0.50, 1, 0)) %>%
  mutate(Topic2 = if_else(Gamma2 > 0.50, 1, 0)) %>%
  mutate(Topic3 = if_else(Gamma3 > 0.50, 1, 0)) %>%
  mutate(Topic4 = if_else(Gamma4 > 0.50, 1, 0))

amazonPosTopics <- topicGammas %>%
  select(-Gamma1, -Gamma2, -Gamma3, -Gamma4)
amazonPosTopics <- left_join(amazonPosTopics, train_lem) %>%
  filter(!is.na(Product)) %>%
  select(-Review)

# amazonPosTopics <- amazonPosTopics %>%
#   mutate(quarter = as.Date(as.yearqtr(Date))) %>%
#   group_by(quarter) %>%
#   summarize(Topic1n = sum(Topic1), Topic2n = sum(Topic2),
#             Topic3n = sum(Topic3), Topic4n = sum(Topic4)) %>%
#   mutate(quartersum = Topic1n+Topic2n+Topic3n+Topic4n) %>%
#   mutate(Topic1prop = Topic1n/quartersum) %>%
#   mutate(Topic2prop = Topic2n/quartersum) %>%
#   mutate(Topic3prop = Topic3n/quartersum) %>%
#   mutate(Topic4prop = Topic4n/quartersum)
  
amazonPosTopics <- amazonPosTopics %>%
  mutate(quarter = as.Date(as.yearqtr(Date))) %>%
  add_count(quarter) %>%
  group_by(quarter) %>%
  mutate(Topic1n = sum(Topic1)) %>%
  mutate(Topic2n = sum(Topic2)) %>%
  mutate(Topic3n = sum(Topic3)) %>%
  mutate(Topic4n = sum(Topic4)) %>%
  mutate(Topic1prop = Topic1n/n) %>%
  mutate(Topic2prop = Topic2n/n) %>%
  mutate(Topic3prop = Topic3n/n) %>%
  mutate(Topic4prop = Topic4n/n) %>%
  mutate(dups = duplicated(quarter)) %>%
  filter(!dups) %>%
  select(-dups)

write.csv(amazonPosTopics, file.path(getwd(),"positiveTopicsForCharts.csv"))

ggplot(amazonPosTopics) + 
  geom_line(aes(quarter, Topic1prop), color = "Red") + 
  geom_line(aes(quarter, Topic2prop), color = "Blue") + 
  geom_line(aes(quarter, Topic3prop), color = "Green") + 
  geom_line(aes(quarter, Topic4prop), color = "Yellow") +
  geom_point(aes(quarter, Topic1prop), color = "Red", size = 3) +
  geom_point(aes(quarter, Topic2prop), color = "Blue", size = 3) + 
  geom_point(aes(quarter, Topic3prop), color = "Green", size = 3) +
  geom_point(aes(quarter, Topic4prop), color = "Yellow", size = 3)


uberplot <- left_join(amazonPosTopics, amazon_time_series) %>%
  filter(!is.na(Product)) %>%
  group_by(quarter) %>%
  mutate(dups = duplicated(quarter)) %>%
  filter(!dups) %>%
  select(-dups)
ones_amazon <- seq(1,1,length.out=length(amazonPosTopics$quarter))
ggplot(uberplot) + 
  geom_area(aes(quarter, 1), fill = "#5ea34d") + 
  geom_area(aes(quarter, 1-avg_sentiment), fill = "#b54747") + 
  geom_line(aes(quarter, Topic1prop), color = "Red") + 
  geom_line(aes(quarter, Topic2prop), color = "Blue") + 
  geom_line(aes(quarter, Topic3prop), color = "Green") + 
  geom_line(aes(quarter, Topic4prop), color = "Yellow") 

ggplot(amazon_time_series) + 
  geom_area(aes(quarter, ones_amazon), fill = "#5ea34d") + 
  geom_area(aes(quarter, 1-avg_sentiment), fill = "#b54747") 


# ggplot(amazon_time_series) + 
#   geom_area(aes(quarter, ones_amazon), fill = "#5ea34d") + 
#   geom_area(aes(quarter, 1-avg_sentiment), fill = "#b54747") + 
#   geom_line(aes(quarter, 1-amazon_time_series_prediction$avg_prediction), 
#             colour = "black", size = 2)

 
amazonPosTopics %>%
  group_by(Index) %>%
  gather(`Topic1`, `Topic2`, `Topic3`, `Topic4`, key = "Topic", value = "Present") %>%
  # mutate(Product = reorder(Product, gamma * topic)) %>%
  ggplot(aes(Topic, Stars)) +
    geom_jitter(position = position_jitter(height = .1),
              alpha = .05)


app_documents %>%
  #mutate(Product = reorder(Product, gamma * topic)) %>%
  ggplot(aes(topic, gamma)) +
    geom_jitter(position = position_jitter(height = .1),
              aes(shape = Product,
                  color = Product),
              alpha = .05)
# clearly we don't have great predictive power to identify apps based on the reviews


           
### code to run topic model
# for n most predictive models, rerurn to explore further
# turn tidy framework into dfm, then add tf_idf, then cast as dtm for topicmodels
train_dfm <- big_td %>%
  select(-Date, -Stars) %>%
  cast_dfm(term = token, document = Index, value = n) %>%
  dfm(remove_punct = TRUE, remove_numbers = TRUE, tolower = TRUE) %>%
  dfm_trim(min_count = 4, max_docfreq = 1000, verbose = TRUE) %>%
  tfidf() 

if (require(topicmodels)) {
    ldaModel <- LDA(convert(train_dfm, to = "topicmodels"), k = 4, 
                    control = list(seed = 1234))
    ihr_terms <- get_terms(ldaModel, 10)
}


# extracting the per-topic-per-word probabilities, called β (“beta”), from the model
# i.e., probability a given token will appear in a given topic
total_topics <- tidy(ldaModel, matrix = "beta")
total_topics

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
  
# examine the per-document-per-topic probabilities, called γ (“gamma”)
# this is essentially a classificiation model!
total_documents <- tidy(ldaModel, matrix = "gamma")
total_documents$document <- as.integer(total_documents$document)
big_td$Product <- as.factor(big_td$Product)
app_documents <- big_td %>%
  select(Index, Product) %>%
  right_join(total_documents, by = c("Index" = "document")) %>%
  group_by(Index) %>%
  #mutate(dups = duplicated(Index)) %>%
  #filter(!dups) %>%
  #select(-dups) %>%
  ungroup() %>%
  mutate(score = topic * gamma)

app_documents %>%
  group_by(Index) %>%
  mutate(Product = reorder(Product, gamma * topic)) %>%
  ggplot(aes(fill = factor(Product), gamma)) +
  geom_histogram(bins = 4) +
  facet_wrap(~ topic)
# clearly we don't have great predictive power to identify apps based on the reviews

app_documents %>%
  #mutate(Product = reorder(Product, gamma * topic)) %>%
  ggplot(aes(topic, gamma)) +
    geom_jitter(position = position_jitter(height = .1),
              aes(shape = Product,
                  color = Product),
              alpha = .05)
# clearly we don't have great predictive power to identify apps based on the reviews

#--------------------------------------------------------------------------------------------------
### Identify negative work frequency

library(zoo)

source("readin.R")

# Import data w/ appropriate col typing
train_lem <- readin(filename="train_lem.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_lem) <- c('Index','Product', 'Date', 'Stars','Review')
train_lem$Product <- as.factor(train_lem$Product)
train_lem$Date <- as.Date(train_lem$Date)

dfm_df <- big_td %>%
  cast_dfm(term = token, document = Index, value = n) %>%
  dfm(remove_punct = TRUE, remove_numbers = TRUE, tolower = TRUE) %>%
  dfm_trim(min_count = 4, max_docfreq = 1000, verbose = TRUE) %>%
  tfidf() 

dfm_df <- big_td %>%
  select(-n) %>%
  
  spread/gather/unite...

amazonNegTopics <- train_lem %>%
  filter(Stars < 4) %>%
  mutate(store = if_else(grepl(" store ", Review, perl=T, ignore.case=T), 1, 0)) %>%
  mutate(sd = if_else(grepl(" sd ", Review, perl=T, ignore.case=T), 1, 0)) %>%
  mutate(crash = if_else(grepl(" crash ", Review, perl=T, ignore.case=T), 1, 0)) %>%
  mutate(horrible = if_else(grepl(" horrible ", Review, perl=T, ignore.case=T), 1, 0)) %>%
  mutate(commercial = if_else(grepl(" commercial ", Review, perl=T, ignore.case=T), 1, 0)) %>%
  select(-Review) 

amazonNegTopics <- amazonNegTopics %>%
  mutate(quarter = as.Date(as.yearqtr(Date))) %>%
  group_by(quarter) %>%
  add_count(quarter) %>%
  mutate(storen = sum(store), sdn = sum(sd), crashn = sum(crash), 
         horriblen = sum(horrible), commercialn = sum(commercial)) %>%
  mutate(storeprop = storen/n, sdprop = sdn/n, crashprop = crashn/n,
         horribleprop = horriblen/n, commercialprop = commercialn/n)

amazonNegWords <- amazonNegTopics %>%
  select(quarter,storeprop,crashprop,horribleprop,commercialprop) %>%
  mutate(dups = duplicated(quarter)) %>%
  filter(!dups) %>%
  select(-dups)

write.csv(amazonNegWords, file.path(getwd(),"negativeWordsForCharts.csv"))

ggplot(amazonNegTopics) + 
  geom_line(aes(quarter, storen), color = "Red") + 
  geom_line(aes(quarter, sdn), color = "Blue") + 
  geom_line(aes(quarter, crashn), color = "Green") + 
  geom_line(aes(quarter, horriblen), color = "Yellow") +
    geom_line(aes(quarter, commercialn), color = "Purple") +
  geom_point(aes(quarter, storen), color = "Red", size = 3) +
  geom_point(aes(quarter, sdn), color = "Blue", size = 3) + 
  geom_point(aes(quarter, crashn), color = "Green", size = 3) +
  geom_point(aes(quarter, horriblen), color = "Yellow", size = 3) +
  geom_point(aes(quarter, commercialn), color = "Purple", size = 3)

ggplot(amazonNegTopics) + 
  geom_line(aes(quarter, storeprop), color = "Red") + 
  geom_line(aes(quarter, sdprop), color = "Blue") + 
  geom_line(aes(quarter, crashprop), color = "Green") + 
  geom_line(aes(quarter, horribleprop), color = "Yellow") +
    geom_line(aes(quarter, commercialprop), color = "Purple") +
  geom_point(aes(quarter, storeprop), color = "Red", size = 3) +
  geom_point(aes(quarter, sdprop), color = "Blue", size = 3) + 
  geom_point(aes(quarter, crashprop), color = "Green", size = 3) +
  geom_point(aes(quarter, horribleprop), color = "Yellow", size = 3) +
  geom_point(aes(quarter, commercialprop), color = "Purple", size = 3)


#--------------------------------------------------------------------------------------------------

# Similarities between apps or star ratings

# https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html#corpus-management-tools 

# turn tidy framework into dfm, then add tf_idf, then cast as dtm for topicmodels
train_dfm <- big_td %>%
  # select(-Date, -Stars) %>%
  cast_dfm(term = token, document = Index, value = n)
  docvars(train_dfm, "Index") <- big_td$Index
  docvars(train_dfm, "Product") <- big_td$Product
  docvars(train_dfm, "Date") <- big_td$Date
  docvars(train_dfm, "Stars") <- big_td$Stars
train_dfm <- train_dfm %>%
  dfm(remove_punct = TRUE, remove_numbers = TRUE, tolower = TRUE) %>%
  dfm_trim(min_count = 4, max_docfreq = 1000, verbose = TRUE) %>%
  tfidf() 

# similarities btwn documents
  ### how can we run this per-app?  need to group by app-document?
docSimil <- textstat_simil(train_dfm, margin="documents", method="cosine")

# hierarchical clustering - get distances on normalized dfm
docDistMat <- textstat_dist(dfm_weight(train_dfm, "relfreq"))
# hiarchical clustering the distance object
docCluster <- hclust(docDistMat)
# label with document names
docCluster$labels <- docnames(train_dfm)
# plot as a dendrogram
plot(docCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")
  # need to simplify the data!!!

#--------------------------------------------------------------------------------------------------
### Standard EOF
# Detach data from search path!
detach() # Repeat as necessary

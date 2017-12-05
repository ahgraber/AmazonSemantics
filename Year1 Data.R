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

# create training and testing set
train_df <- c_data_df %>%
  filter(date < 2013)
test_df <- c_data_df %>%
  filter(date >= 2014)

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

token_Stars <- tokenValues(big_td, "Index")

#--------------------------------------------------------------------------------------------------
### Create Sentiment model

#--------------------------------------------------------------------------------------------------
### Predictive Model

source("modifier_gen.R")

# create dictionary consisting of token, average rating, and sentiment code
seed <- token_Stars %>%
  count(token, avg_rating) %>%
  mutate(dups = duplicated(token)) %>%
  filter(dups == FALSE) %>%
  select(-dups) %>%
  filter(n > 5)

# add sentiment code (-1, 0, 1)
sentiment_dictionary <- seed %>%
  mutate(modifier = modifier_gen(seed$avg_rating, 3.1, 2.9)) %>%
  select(-n)
  ### note - I tested about 40 combinations by hand to arrive at this - 
  ### there's got to be an optimization function that we could use for this...

write.csv(sentiment_dictionary, 
          file.path(paste(getwd(),"Lists",sep = "/"),"sentiment_dictionary.csv"),
          row.names = F)

rm(seed)
rm(sentiment_dictionary)
#--------------------------------------------------------------------------------------------------

# read in dictionary and filter for tokens in it
sentiment_dictionary <- readin(filename="sentiment_dictionary.csv", subfolder="Lists", infolder=TRUE)
#model_dictionary <- model_dictionary[,-1]
#model_dictionary <- model_dictionary %>%
#  select(Index, token, Product, Stars, modifier)
#train_lem_model <- train_lem %>%
#  count(Index, word, Stars)

# join the dictionary's star rating modifiers to the bag of words
train_model <- big_td %>%
  left_join(sentiment_dictionary) %>%
  group_by(Index) %>%
  mutate(count = n()-1) %>%
  mutate(sentiment = case_when(
         Stars >= 4 ~ 1,          # 4,5 = positive
         Stars < 4 ~ 0)) %>%      # 1-3 = negative
  ungroup()
  
# sum the modifiers over each review, divide by number of tokens per review for a 'score'
review_aggregate <- train_model %>%
  group_by(Index) %>%
  summarise(modifier = sum(modifier, na.rm = TRUE), 
            Stars = mean(Stars), 
            rating = mean(sentiment), 
            count = n()) %>%
  mutate(score = modifier/count) %>%
  ungroup()

# visualize
ggplot(review_aggregate, aes(score, rating)) + 
  geom_jitter(position = position_jitter(height = .1),
              alpha = .05)


# create linear regression model between the score and the star rating
  ##### what about glm? or other non-linear model?
model <- lm(rating~score, review_aggregate)
summary(model)
AIC(model)
# Residual standard error: 0.3319 on 15500 degrees of freedom
# Multiple R-squared:  0.2365,	Adjusted R-squared:  0.2364 
# F-statistic:  4801 on 1 and 15500 DF,  p-value: < 2.2e-16
# AIC: 9797.19


# logistic regression
model2 <- glm(rating~score, review_aggregate, family=binomial())
summary(model2)
#     Null deviance: 14365  on 15501  degrees of freedom
# Residual deviance: 11297  on 15500  degrees of freedom
# AIC: 11301


  ##### does prediction improve if we code ratings as pos/neg/neutral?
cor(review_aggregate$rating,review_aggregate$score)
# 0.4862863

#--------------------------------------------------------------------------------------------------
### Validate model

source("readin.R")
source("firstClean.R")
source("findTypos.R")
source("fixTypos.R")
source("monograms.R")
source("bigrams.R")
source("trigrams.R")
source("tetragrams.R")
library(tidyverse)
library(tidytext)
library(textstem)


# create test data
test_df <- readin(filename="test.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(test_df) <- c('Index','Product', 'Date', 'Stars','Review')
test_df$Product <- as.factor(test_df$Product)
test_df$Date <- as.Date(test_df$Date)

test_lem <- test_df
toclean <- test_df$Review

# clean reviews and app
  # replace_number seems to be causing problems
cleaned1 <- firstClean(toclean)
# use to find and replace common typos
cleaned2 <- fixTypos(cleaned1)
# lemmatize
cleaned3 <- lemmatize_strings(cleaned2, dictionary = lexicon::hash_lemmas)

test_lem$Review <- cleaned3
colnames(test_lem) <- c('Index','Product', 'Date', 'Stars','Review')
rm(cleaned1, cleaned2, cleaned3)

# replace column of reviews with tidy column of tokens
monograms <- monograms(test_lem)
bigrams <- bigrams(test_lem)
trigrams <- trigrams(test_lem)
tetragrams <- tetragrams(test_lem)

# join monogram, bigram, trigram, tetragram(?)
test_td <- rbind(monograms,bigrams,trigrams,tetragrams)
rm(monograms,bigrams,trigrams,tetragrams)

# bind test data with dictionary
test_td_model <- left_join(test_td, sentiment_dictionary)

# create test aggregation
review_aggregate_test <- test_td_model %>%
  group_by(Index) %>%
  mutate(sentiment = case_when(
         Stars >= 4 ~ 1,          # 4,5 = positive
         Stars < 4 ~ 0)) %>%      # 1-3 = negative
  summarise(modifier = sum(modifier, na.rm = TRUE), 
            Stars = mean(Stars),
            rating = mean(sentiment),
            count = n()) %>%
  mutate(score = modifier/count) %>%
  mutate(prediction = predict(model, newdata=data_frame(score))) %>%
  mutate(deltas = prediction-rating) %>%
  mutate(whole_rating = round(prediction,0)) %>%
  mutate(accuracy = if_else((whole_rating-rating)==0, 1, 0))

sum(review_aggregate_test$accuracy) / nrow(review_aggregate_test)
# 84.6% !!

ggplot(review_aggregate_test, aes(prediction, rating)) + geom_point()
ggplot(review_aggregate_test, aes(deltas, rating)) + geom_point()
ggplot(review_aggregate_test, aes(deltas, prediction)) + geom_point()
cor(review_aggregate_test$rating, review_aggregate_test$prediction)


# test aggregation based on logistic model
# create test aggregation
review_aggregate_test2 <- test_td_model %>%
  group_by(Index) %>%
  mutate(sentiment = case_when(
         Stars >= 4 ~ 1,          # 4,5 = positive
         Stars < 4 ~ 0)) %>%      # 1-3 = negative
  summarise(modifier = sum(modifier, na.rm = TRUE), 
            Stars = mean(Stars),
            rating = mean(sentiment),
            count = n()) %>%
  mutate(score = modifier/count) %>%
  mutate(prediction = predict(model2, newdata=data_frame(score), type = "response")) %>%
  mutate(deltas = prediction-rating) %>%
  mutate(whole_rating = round(prediction,0)) %>%
  mutate(accuracy = if_else((whole_rating-rating)==0, 1, 0))

sum(review_aggregate_test2$accuracy) / nrow(review_aggregate_test2)
# 84.6% !!!
cor(review_aggregate_test2$rating, review_aggregate_test2$prediction)

#--------------------------------------------------------------------------------------------------
### Time series analysis

library(zoo)

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
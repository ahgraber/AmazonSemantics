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
  mutate(modifier = modifier_gen(seed$avg_rating,3.85, 3.05)) %>%
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
  ungroup()
  
# sum the modifiers over each review, divide by number of tokens per review for a 'score'
review_aggregate <- train_model %>%
  group_by(Index) %>%
  summarise(modifier = sum(modifier, na.rm = TRUE), Stars = mean(Stars), count = n()) %>%
  mutate(score = modifier/count) %>%
  ungroup()

# visualize
ggplot(review_aggregate, aes(score, Stars)) + 
  geom_jitter(position = position_jitter(height = .1),
              alpha = .05)


# create linear regression model between the score and the star rating
  ##### what about glm? or other non-linear model?
model <- lm(review_aggregate$Stars~review_aggregate$score)
summary(model)
coefs <- coef(model)
# Residual standard error: 0.9664 on 17392 degrees of freedom
# Multiple R-squared:  0.3766,	Adjusted R-squared:  0.3765 
# F-statistic: 1.051e+04 on 1 and 17392 DF,  p-value: < 2.2e-16

  ##### does prediction improve if we code ratings as pos/neg/neutral?
cor(review_aggregate$Stars,review_aggregate$score)
# 0.613653

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
  summarise(modifier = sum(modifier, na.rm = TRUE), Stars = mean(Stars), count = n()) %>%
  mutate(score = modifier/count) %>%
  mutate(star_prediction = coefs[1] + coefs[2]*score) %>%
  mutate(deltas = star_prediction-Stars) %>%
  mutate(whole_stars = round(star_prediction,0)) %>%
  mutate(accuracy = if_else((whole_stars-Stars)==0, 1, 0))

sum(review_aggregate_test$accuracy) / nrow(review_aggregate_test)

ggplot(review_aggregate_test, aes(star_prediction, Stars)) + geom_point()
ggplot(review_aggregate_test, aes(deltas, Stars)) + geom_point()
ggplot(review_aggregate_test, aes(deltas, star_prediction)) + geom_point()
cor(review_aggregate_test$Stars, review_aggregate_test$star_prediction)

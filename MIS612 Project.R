# MIS612 Class Project
# Main Script

#--------------------------------------------------------------------------------------------------
### Install packages

# Uncomment and run the first time to ensure all packages are installed

###* note "##" means we did not end up using this package

# install.packages("tidyverse")
# install.packages("textclean")
# install.packages("quanteda")
# install.packages("stringr")
# install.packages("NLP")
# install.packages("reshape2") 
# install.packages('stm')

# install.packages("tidytext")

## install.packages("qdap")
## install.packages("textstem")
## install.packages("tm")
## install.packages('RQDA')

## install.packages("RSentiment") # see: https://cran.r-project.org/web/packages/RSentiment/vignettes/Introduction.html
## install.packages("SentimentAnalysis") # see: https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
## install.packages("sentiment") # unavailable for R 3.4.1

#--------------------------------------------------------------------------------------------------
### Collect Data
source("scrapeAmazon.R")

# What to scrape?  *** Scraping steaming music apps - NOTE: AMAZON/ANDROID ONLY ***

# # Amazon: https://www.amazon.com/product-reviews/B004FRX0MY/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# amazon.df <- scrapeAmazon("Amazon", "B004FRX0MY", 1379, delay=2)
# # Save data 
# write.csv(amazon.df, "Amazon.csv")
# 
# # iHeartRadio: https://www.amazon.com/product-reviews/B005ZFOOE8/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# iheartradio.df <- scrapeAmazon("iHeartRadio", "B005ZFOOE8", 1384, delay=2)
# # Save data 
# write.csv(iheartradio.df, "iHeartRadio.csv")
# 
# # Pandora: https://www.amazon.com/product-reviews/B005V1N71W/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# pandora.df <- scrapeAmazon("Pandora", "B005V1N71W", 1979, delay=2)
# # Save data 
# write.csv(pandora.df, "Pandora.csv")
# 
# # Spotify: https://www.amazon.com/product-reviews/B00KLBR6IC/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
# spotify.df <- scrapeAmazon("Spotify", "B00KLBR6IC", 1504, delay=2)
# # Save data 
# write.csv(spotify.df, "Spotify.csv")

# # Aggregate all data into master file
# data.df <- rbind(amazon.df, iheartradio.df, pandora.df, spotify.df)
# write.csv(data.df, file.path(paste(getwd(),"Scraped Data",sep="/"),"data.csv"))

#--------------------------------------------------------------------------------------------------
### Raw Data Cleaning

# Initialize scripts
source("readin.R")
source("firstClean.R")

# Import data w/ appropriate col typing
data.df <- readin(filename="data.csv", folder="Scraped Data", infolder=TRUE)

# create data frame of stuff not-to-clean (dates, stars, etc)
preserve.df <- cbind.data.frame(data.df$prod, data.df$date, data.df$stars,  ## NOTE: DROPS AUTHOR!!!
                                stringsAsFactors=FALSE)  

colnames(preserve.df) <- c('Product', 'Date', 'Stars')
preserve.df$Product <- as.factor(preserve.df$Product)
preserve.df$Date <- as.Date(preserve.df$Date, format="%B%d,%Y")

# create data frame of just review to be cleaned
toclean.df <- as.data.frame(paste(data.df$title," ",data.df$comments))

# clean reviews and app
cleaned.df <- firstClean(toclean.df)

# combine reviews and formatted other stuff
c.data.df <- cbind(preserve.df,cleaned.df)
colnames(c.data.df) <- c('Product', 'Date', 'Stars','Review')
c.data.df$Product <- as.factor(c.data.df$Product)
c.data.df$Date <- as.Date(c.data.df$Date, format="%B%d,%Y")

# save cleaned data
write.csv(c.data.df, file.path(dataPath,"c.data.csv"))

#--------------------------------------------------------------------------------------------------
### Create Training Set

# Initialize scripts
source("readin.R")
source("randomSample.R")

# Import data w/ appropriate col typing
c.data.df <- readin(filename="c.data.csv", folder="Scraped Data", infolder=TRUE)
colnames(c.data.df) <- c('Product', 'Date', 'Stars','Review')
c.data.df$Product <- as.factor(c.data.df$Product)
c.data.df$Date <- as.Date(c.data.df$Date)

# Randomly select 30 percent of the data and store it in a data frame
training.df <- randomsample(c.data.df, 30)

# save training subset
write.csv(training.df, file.path(dataPath,"training.csv"))

#--------------------------------------------------------------------------------------------------

### Corpus Generation

# Initialize scripts
source("readin.R")
source("Corpus.R") 

# Import data w/ appropriate col typing
training.df <- readin(file.path(dataPath,"training.csv"))
colnames(training.df) <- c('Product', 'Date', 'Stars','Review')
training.df$Product <- as.factor(training.df$Product)
training.df$Date <- as.Date(training.df$Date)

#
# script to generate corpus, dtm
#


#-------------------------------------------------------------------------------------------------- 
### Text Mining 
  ## First, explore the open responses using the Text Mining package and look for frequently
  ## occurring words, associations between words as well as other text mining functions.

library(quanteda)
library(reshape2)
library(textstem)

## Import data file (if needed)
# cdata.df <- read.csv ("cdata.csv", stringsAsFactors=FALSE)

# Create a corpus for text mining
cdata.corpus <- Corpus(DataframeSource (cdata.df))

# Eliminate extra spaces
cdata.corpus <- tm_map(cdata.corpus, content_transformer(stripWhitespace))

# Force lower case
cdata.corpus <- tm_map(cdata.corpus, content_transformer(tolower)) 

# Remove Punctuation
cdata.corpus <- tm_map(cdata.corpus, content_transformer(removePunctuation))

# build a document-term matrix
## do we want a document-term or term-document matrix??
  # **DocumentTermMatrix create a matrix with documents as rows and terms as columns
  # TermDocumentMatrix create a matrix with terms as rows and documents as columns
cdata.dtm <- DocumentTermMatrix(cdata.corpus, control = list(stopwords=stopwords("en"), 
                                                             stemming=FALSE))  # can we lemmatize?
# Lemmatize vs stem: try lemmatize_words() from textstem package

# Shows cases where "success" appears. 
# If "Subscript out of bounds" error, word does not appear
melt(inspect (cdata.dtm [,"success"])) 

# inspect most popular words
findFreqTerms(cdata.dtm, lowfreq=10) ##Terms that appear 10+ times 

# Counts for top words
freqwrds <- sort (colSums (as.matrix(mydata)),decreasing=TRUE)

# Returns top 100 words
melt(freqwrds [1:100])

# Associations of word "success" with other terms
findAssocs(cdata.dtm, "success", 0.2)

#--------------------------------------------------------------------------------------------------
### Thematic coding with RQDA

# AG Note: this isn't necessary yet.

### Following instructions from: 
  # https://dugontario.files.wordpress.com/2013/12/qualitative-analysis-in-r.pdf 

# Install RQDA
  ## Note: installation was REALLY difficult on my mac - 
  ## may have to follow instructions from 'sanjulr' here: https://gist.github.com/sebkopf/9405675
  # install.packages('RQDA')

# library(RQDA)
# 
# RQDA()
# codingBySearch("student", ##word or phrase you want to search
#                fid="1",##FileID, from GUI
#                cid="1") ##CodeID, from GUI

#--------------------------------------------------------------------------------------------------
### Sentiment analysis with RSentiment / SentimentAnalysis
# reference: https://sites.google.com/site/miningtwitter/basics/text-mining

# Install packages
  # install.packages("RSentiment") # see: https://cran.r-project.org/web/packages/RSentiment/vignettes/Introduction.html
  # install.packages("SentimentAnalysis") # see: https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
  ## install.packages("sentiment") # unavailable for R 3.4.1
library(RSentiment)
library(SentimentAnalysis)

## Using RSentiment
score <- calculate_score(cdata.df)
score
sentiment <- calculate_sentiment(cdata.df)
sentiment
totalSent <- calculate_total_presence_sentiment(cdata.df)
totalSent

#--------------------------------------------------------------------------------------------------
### Sentiment analysis with (?) tidytext

# AG Note: tidytext requires data in a different format from previous.  

# Install packages
  # install.packages("tidyverse")
  # install.packages("tidytext") # see: https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
library(tidytext)


#--------------------------------------------------------------------------------------------------
### Standard EOF
# Detach data from search path!
detach() # Repeat as necessary

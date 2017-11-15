# MIS612 Class Project
# Main Script

#--------------------------------------------------------------------------------------------------
### Install packages

  # Uncomment and run the first time to ensure all packages are installed

  # install.packages("tidyverse")
  # install.packages("pacman")
  # install.packages("qdap")
  # install.packages("tm")
  # install.packages("reshape")
  # install.packages('RQDA')
  # install.packages("tidytext")
  # install.packages("RSentiment") # see: https://cran.r-project.org/web/packages/RSentiment/vignettes/Introduction.html
  # install.packages("SentimentAnalysis") # see: https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
  ## install.packages("sentiment") # unavailable for R 3.4.1

#--------------------------------------------------------------------------------------------------
### Collect Data

# see scrapeAmazon.R

#--------------------------------------------------------------------------------------------------
### Raw Data Cleaning

# see firstClean.R

#--------------------------------------------------------------------------------------------------
### Aggregate data

# Read in cleaned data

# Stack individual frames into single large

# Combine $title and $comment

# Recognize date as date

# Drop user

# Create version with and without star ratings
  # Note: this may really fuck us if we don't keep both datasets up to date.  


#--------------------------------------------------------------------------------------------------
### Create Training Set




#-------------------------------------------------------------------------------------------------- 
### Text Mining 
  ## First, explore the open responses using the Text Mining package and look for frequently
  ## occurring words, associations between words as well as other text mining functions.

# Install text mining and reshape packages
  # install.packages("tm")
  # install.packages("reshape")
library(tm)  # see: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
library(reshape)

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
freqwrds <- sort (colSums (as.matrix(cdata.dtm)),decreasing=TRUE)

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

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
# install.packages("textstem")
# install.packages("reshape2") 
# install.packages("stm")
# install.packages("hunspell")
# install.packages("NLP")

# install.packages("devtools")
# devtools::install_github("ahgraber/tmt")

# install.packages("tidytext")

## install.packages("tm")
## install.packages("qdap")
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
write.csv(c.data.df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"c.data.csv"))

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
training.df <- randomSample(c.data.df, 30)

# save training subset
write.csv(training.df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"training.csv"))

#--------------------------------------------------------------------------------------------------
### Training Data

# Initialize scripts
library(quanteda)
library(textstem)

#library(stringr)
# library(tm)
source("readin.R")

# Import data w/ appropriate col typing
training.df <- readin(filename="training.csv", folder="Scraped Data", infolder=TRUE)
colnames(training.df) <- c('Product', 'Date', 'Stars','Review')
training.df$Product <- as.factor(training.df$Product)
training.df$Date <- as.Date(training.df$Date)

#-------------------------------------------------------------------------------------------------- 
### check for typos - currently not working.  output is no different from input

# see fixTypos1.R and fixTypos2.R

# fixTypos2 is probably the best option at this point, but requires external program installation
# see notes in fixTypos2.R for further information:
  # Based on review, we either need to improve dictionary to update for 2017-era internet 
  # terminology, or we can just continue without spelling fixes.  This will decrease reliability 
  # a little bit (we'll miss spelling variants of "Spotify", etc) but getting reliable spelling 
  # fixes is probably not worth the amount of time required to fix
    # AND
  # the majority of the common misspellings we can probably accomodate via synonym identification

#-------------------------------------------------------------------------------------------------- 
### Create corpora

# creating corpus split at every space
textbag <- corpus(training.df$Review)
  # add potentially relevant additional data back to corpus
  docvars(textbag, "Product") <- training.df$Product
  docvars(textbag, "Date") <- training.df$Date
  docvars(textbag, "Stars") <- training.df$Stars

  summary(textbag)

# creating corpus split at every space with lemmas
lembag <- corpus(lemmatize_strings(training.df$Review,dictionary = lexicon::hash_lemmas))
  # add potentially relevant additional data back to corpus
  docvars(lembag, "Product") <- training.df$Product
  docvars(lembag, "Date") <- training.df$Date
  docvars(lembag, "Stars") <- training.df$Stars

  summary(lembag)

#-------------------------------------------------------------------------------------------------- 
### Stopword management
  
# see wordListMgmt.R
SMART.list <- readin("SMART_stop.txt", folder="Lists", infolder=T)
en.list <- readin("en_stop.txt", folder="Lists", infolder=T)
stopwords.df <- as.data.frame(c(SMART.list, en.list))
  
#-------------------------------------------------------------------------------------------------- 
### Synonym management
  
# see wordListMgmt.R 
  

#-------------------------------------------------------------------------------------------------- 
### Tokenization
  ### Don't need tokens directly b/c we can pass the same arguments straight through to build a DTM
  
#-------------------------------------------------------------------------------------------------- 
### Document-Term Matrix creation, cleaning, simplification

# Create Doc-Term Matrix from corpus; w/ lemma replacement and stemming, and stopwords removed
docMatrix <- dfm(lembag,
                  remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE,
                  remove = stopwords("english"), stem = TRUE)

# Top 50 words
topfeatures(docMatrix, 50)


# create Doc-Term Matrix grouping by App
docMatrixApp <- dfm(lembag, ## lembag vs textbag
                     groups = "Product",
                     remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE,
                     remove = stopwords("english"), stem = TRUE)

# create Doc-Term Matrix grouping by Star Rating
docMatrixStar <- dfm(lembag, ## lembag vs textbag
                     groups = "Stars",
                     remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE,
                     remove = stopwords("english"), stem = TRUE)


# ngrams and negations https://github.com/kbenoit/quanteda/issues/516

#--------------------------------------------------------------------------------------------------
### Themes & Topic models 

# see:https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html#corpus-management-tools 

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

# see:https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html#corpus-management-tools 

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

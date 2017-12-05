### Function to save & import stopwords list(s) for manual editing

# wordListMgmt 

  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, tidytext)
  
#--------------------------------------------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(quanteda)
source("readin.R")
dataPath <- paste(getwd(),"Lists",sep="/")

#--------------------------------------------------------------------------------------------------
### Save list(s) for review/revision

## Export standard stopwords list for external editing
  spwords <- tidytext::stop_words
  write.csv(spwords, file = paste(dataPath, "stopwords.csv", sep = "/"), row.names = F)

## Lemmas
  # save lemma list for editing (from http://www.lexiconista.com/Datasets/lemmatization-en.zip)
  temp <- tempfile()
  download.file("http://www.lexiconista.com/Datasets/lemmatization-en.zip",temp)
  lemma.list <- read.table(unz(temp, "lemmatization-en.csv"),sep="",quote="")
  colnames(lemma.list) <- c("Lemma","Source")
  write.csv(lemma.list, file = paste(dataPath, "lemmas.csv", sep = "/"), row.names = F)
  unlink(temp)
  
## Synonyms

## Sentiments
  
  # get sentiments 
  bing <- get_sentiments("bing")  # positive/negative
  write.csv(bing, file = paste(dataPath, "bing_sentiment.csv", sep = "/"), row.names = F)
  nrc <- get_sentiments("nrc")    # with emotion
  write.csv(nrc, file = paste(dataPath, "nrc_sentiment.csv", sep = "/"), row.names = F)
  afinn <- get_sentiments("afinn")    # with emotion
  write.csv(afinn, file = paste(dataPath, "afinn_sentiment.csv", sep = "/"), row.names = F)
  
#--------------------------------------------------------------------------------------------------
### Manually edit lists

## Stopwords notes:
  spwords <- readin("stopwords.csv", subfolder="Lists",infolder=T)
  

  # remove words from stop words list
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "not"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "never"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "than"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "problem"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "problems"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "appreciate"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "except"))  
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "work"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "works"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "worked"))
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "working"))
  # keep "not"; look for ngrams involving "not"
  
  # add words to stop words list
  custom_spwords <- bind_rows(data_frame(word = c("nd", "ð", "absolutely", "music", "app",
                                                  "quite", "star", "listen", "love", "song",
                                                  "radio", "artist", "na", "day", "kindle fire",
                                                  "5", "track", "rock", "people", "country", "yay",
                                                  "tune", "2", "facebook", "mp3", "3", "nice", 
                                                  "4", "10", "email", "1", "family", "lot",
                                                  "phone", "kindle"), 
                                         lexicon = "custom"), 
                              custom_spwords)


  # save custom list
  write.csv(custom_spwords, file = paste(dataPath, "custom_spwords.csv", sep = "/"), row.names = F)
  
## Lemmas notes:
  
  
## synonyms notes:

#--------------------------------------------------------------------------------------------------
### Import edited lists

## Stopwords
  spwords <- readin("stopwords.csv",folder="Lists",infolder=T)
  custom_spwords <- readin("custom_spwords.csv",folder="Lists",infolder=T)
 

## Lemmas
  lemma.list <- readin("lemmas.csv", folder="Lists", infolder=T)
    
## Synonyms
  
  
## Sentiments
  bing <- readin("bing_sentiment.csv", folder="Lists", infolder=T)
  nrc <- readin("nrc_sentiment.csv", folder="Lists", infolder=T)
  afinn <- readin("afinn_sentiment.csv", folder="Lists", infolder=T)
  

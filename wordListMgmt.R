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
  write.csv(spwords, file = paste(dataPath, "stopwords.txt", sep = "/"), row.names = F)

## Lemmas
  # save lemma list for editing (from http://www.lexiconista.com/Datasets/lemmatization-en.zip)
  temp <- tempfile()
  download.file("http://www.lexiconista.com/Datasets/lemmatization-en.zip",temp)
  lemma.list <- read.table(unz(temp, "lemmatization-en.txt"),sep="",quote="")
  colnames(lemma.list) <- c("Lemma","Source")
  write.table(lemma.list, file = paste(dataPath, "lemmas.txt", sep = "/"))
  unlink(temp)
  
## Synonyms

## Sentiments
  
  # get sentiments 
  bing <- get_sentiments("bing")  # positive/negative
  write.csv(bing, file = paste(dataPath, "bing_sentiment.txt", sep = "/"), row.names = F)
  nrc <- get_sentiments("nrc")    # with emotion
  write.csv(nrc, file = paste(dataPath, "nrc_sentiment.txt", sep = "/"), row.names = F)
  afinn <- get_sentiments("afinn")    # with emotion
  write.csv(afinn, file = paste(dataPath, "afinn_sentiment.txt", sep = "/"), row.names = F)
  
#--------------------------------------------------------------------------------------------------
### Manually edit lists

## Stopwords notes:
  spwords <- readin("stopwords.txt", subfolder="Lists",infolder=T)
  

  # remove words from stop words list
  custom_spwords <- filter_at(spwords, vars("word"), any_vars(. != "not"))
      # keep "not"; look for ngrams involving "not"
  
  # add words to stop words list
  custom_spwords <- bind_rows(data_frame(word = c("nd", "ð", "absolutely"), 
                                         lexicon = "custom"), 
                              custom_spwords)


  # save custom list
  write.csv(custom_spwords, file = paste(dataPath, "custom_spwords.txt", sep = "/"), row.names = F)
  
## Lemmas notes:
  
  
## replace synonyms notes:
  ## as proven by current typo correction, this does not work currently
  # synonym <- c("awsome", "favourite", "favs", "fav", "reccomend", 
  #           "recomend", "comercials", "dissapointed", "realy", "thx", 
  #           "luv", "bc", "cuz", "excelent", "excelente",
  #           "thier", "mins", "alot", "lil", "panadora", 
  #           "spodify", "dosent", "ive", "Ive", "everytime" )
  # replacement <- c("awesome", "favorite", "favorite", "favorite", "recommend",
  #                  "recommend", "commercials", "disappointed", "really", "thanks",
  #                  "love", "because", "because", "excellent", "excellent",
  #                  "their", "minutes", "a lot", "little", "pandora",
  #                  "spotify", "does not", "I have", "I have", "every time")
  # 
  # so_regex = "/bso*o/b"
  # uh_regex = "/buh*h/b"
#--------------------------------------------------------------------------------------------------
### Import edited lists

## Stopwords
  spwords <- readin("stopwords.txt",folder="Lists",infolder=T)
  custom_spwords <- readin("custom_spwords.txt",folder="Lists",infolder=T)
 

## Lemmas
  lemma.list <- readin("lemmas.txt", folder="Lists", infolder=T)
    
## Synonyms
  
  
## Sentiments
  bing <- readin("bing_sentiment.txt", folder="Lists", infolder=T)
  nrc <- readin("nrc_sentiment.txt", folder="Lists", infolder=T)
  afinn <- readin("afinn_sentiment.txt", folder="Lists", infolder=T)
  

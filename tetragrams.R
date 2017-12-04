tetragrams <- function (tbl_df) {
  
  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, stringr, tidytext)
  
  if(!exists("readin.R", mode="function")) source("readin.R")
  
#-------------------------------------------------------------------------------------------------- 
  
  ### Bigrams

  # import revised stop word list
  custom_spwords <- readin(filename="custom_spwords.csv", subfolder="Lists", infolder=T)

  # create ngram tokens based on the number (nn) of words in the n-gram
  ngram <- tbl_df %>%
    unnest_tokens(token, Review, token = "ngrams", n = 4)
  
  # remove frequently used words from bigrams 
  separated <- ngram %>%
    separate(token, c("word1", "word2", "word3", "word4"), sep = " ")
  
  # remove rows containing a stopword in any column
  filtered <- separated %>%
    filter(!word1 %in% custom_spwords$word) %>%
    filter(!word2 %in% custom_spwords$word) %>%
    filter(!word3 %in% custom_spwords$word) %>%
    filter(!word4 %in% custom_spwords$word)
  
  # recombine with original df
  united <- filtered %>%
    unite(token, word1, word2, word3, word4, sep = " ")
  
  # calculate term frequency
  frequency <- cbind(count(united, token, sort=T))
  
  # keep only most frequent terms
  q <- quantile(frequency$n, .95)
  infreqterms <- frequency %>%
    filter(n<q) %>%
    ungroup()

  # remove infrequent terms
  filtered2 <- anti_join(frequency, infreqterms)
  
  # add frequency to full tbl
  tetragrams <- left_join(united,filtered2) %>%
    filter(!is.na(n))
  
  return(tetragrams)
  
  return(filtered2)

}  

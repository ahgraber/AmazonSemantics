monograms <- function (tbl_df) {
  
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
    unnest_tokens(token, Review, token = "ngrams", n = 1)
  
  separated <- ngram
  
  # remove rows containing a stopword in any column
  filtered <- separated %>%
    filter(!token %in% custom_spwords$word)
  
  # recombine with original df
  united <- filtered 
  
  # calculate term frequency
  frequency <- cbind(count(united, token, sort=T))
  
  # keep only most frequent terms
  q <- quantile(frequency$n, .8)
  infreqterms <- frequency %>%
    filter(n<q) %>%
    ungroup()

  # remove infrequent terms
  filtered2 <- anti_join(frequency, infreqterms)
  
  # add frequency to full tbl
  monograms <- left_join(united,filtered2) %>%
    filter(!is.na(n))
  
  return(monograms)

}  

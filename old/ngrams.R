ngrams <- function (tbl_df, nn) {

  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, stringr, tidytext)
  
  if(!exists("readin.R", mode="function")) source("readin.R")

#-------------------------------------------------------------------------------------------------- 
### n-grams

  # import revised stop word list
  custom_spwords <- readin(filename="custom_spwords.csv", subfolder="Lists", infolder=T)
  
  # create ngram tokens based on the number (nn) of words in the n-gram
  ngram <- tbl_df %>%
    unnest_tokens(grams, Review, token = "ngrams", n = nn)
  
  # remove frequently used words from bigrams 
  cols <- as_data_frame()
  
  for (i in nn) {
      cols <- c(cols, paste0("word",i))
  }
  separated <- ngram %>%
    separate(grams, c(rep(paste0("word",))), sep = " ")
  
  # remove rows containing a stopword in any column
  filtered <- separated %>%
    filter(!word1 %in% custom_spwords$word) %>%
    filter(!word2 %in% custom_spwords$word)
  
  # recombine with original df
  united <- filtered %>%
    unite(grams, word1, word2, sep = " ")
  
  # drop infrequent terms (<5)
  frequency <- cbind(count(united, grams, sort=T))
  
  # look at infrequently used terms per product
  infreqterms <- frequency %>%
    filter(n<5) %>%
    ungroup()

  # remove infrequent terms
  filtered2 <- anti_join(frequency, infreqterms)
  
  return(filtered2)

}

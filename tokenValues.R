tokenValues <- function(tbl_df, group) {
  # where tbl_df is data frame, group is column we want to group by
  
    # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, stringr, tidytext)
  
  if(!exists("readin.R", mode="function")) source("readin.R")
  
#-------------------------------------------------------------------------------------------------- 
### pass tbl_df with tokens, ratings, group to return tokVals with:
  # avg_rating per token (overall)
  # avg_stdev per token (overall)
  # tf, idf, tf-idf per token (based on group)

  # create column with frequency, mean, stdev
  tokVals <- tbl_df %>%
    select(Index, token, Product, Stars) %>%
    group_by(token) %>%
    mutate(frequency = n()) %>%
    mutate(avg_rating = mean(Stars)) %>%
    mutate(avg_stdev = sd(Stars))
  
  # filter out words with a standard deviation of ratings greater than one but not 0
  tokVals <- tokVals %>%
    filter(avg_stdev < 1) %>%
    filter(avg_stdev > 0)
  
  tokVals <- tokVals %>%
    bind_tf_idf(token, !!group, frequency) %>%
    ungroup()
  
  # remove duplicates
  tokVals <- tokVals %>%
    mutate(dups = duplicated(tokVals)) %>%
    filter(dups == FALSE) %>%
    select(-dups)

  return(tokVals)
  
}
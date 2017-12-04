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
    mutate(avg_stdev = sd(Stars)) %>% 
    bind_tf_idf(token, !!group, frequency) %>%
    ungroup()
  
  cor(tokVals$Stars, tokVals$avg_rating)
   
  ggplot(tokVals, aes(avg_rating)) + geom_histogram()
  ggplot(tokVals, aes(avg_stdev)) + geom_histogram()
  ggplot(tokVals, aes(tf)) + geom_histogram()
  ggplot(tokVals, aes(idf)) + geom_histogram()
  ggplot(tokVals, aes(tf_idf)) + geom_histogram()

  # filter out words with a standard deviation of ratings greater than one but not 0
  tokVals <- tokVals %>%
    filter(avg_stdev < 1) %>%
    filter(avg_stdev > 0)

  cor(tokVals$Stars, tokVals$avg_rating)
  
  # filter out the lowest 25% idf (i.e., 25% most frequent terms)
  q <- quantile(tokVals$tf_idf, .25)
  tokVals <- tokVals %>%
    filter(idf > q)
  
  cor(tokVals$Stars, tokVals$avg_rating)
  
  # remove duplicates
  tokVals <- tokVals %>%
    mutate(dups = duplicated(tokVals)) %>%
    filter(dups == FALSE) %>%
    select(-dups)

  cor(tokVals$Stars, tokVals$avg_rating)
  
  return(tokVals)
  
}
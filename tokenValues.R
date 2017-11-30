tokenValues <- function(tbl_df, group) {
  
    # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, stringr, tidytext)
  
  if(!exists("readin.R", mode="function")) source("readin.R")
  
#-------------------------------------------------------------------------------------------------- 
### pass tbl_df with tokens, ratings, group to return a tbl_df with:
  # avg_rating per token (overall)
  # avg_stdev per token (overall)
  # tf, idf, tf-idf per token (based on group)
 
  
}
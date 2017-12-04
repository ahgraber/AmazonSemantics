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
    bind_tf_idf(token, Index, frequency) %>%
    ungroup()
  
  tokVals <- tokVals %>%
    group_by(Stars, token) %>%
    mutate(starFreq = n()) %>%
    ungroup()
  
  # tokVals <- tokVals %>%
  #   group_by(token) %>%
  #   mutate(wtavg_rating = weighted.mean(Stars,starFreq)) %>%
  #   ungroup()
  
  tokVals <- tokVals %>%
    group_by(token) %>%
    mutate(avg_err = avg_rating - Stars) %>%
  #  mutate(wtavg_err = wtavg_rating - Stars) %>%
    ungroup()
  
  # # check initial correlations between actual and average ratings (higher is better)
  # cor1 <- cor(tokVals$Stars, tokVals$avg_rating)
  # cor2 <- cor(tokVals$Stars, tokVals$wtavg_rating)
  # 
  # plot1 <- ggplot(tokVals, aes(Stars, avg_err)) + 
  #   geom_jitter(position = position_jitter(width = .1),
  #               alpha = .05) + 
  #   scale_y_continuous(limits=c(-5, 5))
  # plot2 <- ggplot(tokVals, aes(Stars, wtavg_err)) + 
  #   geom_jitter(position = position_jitter(width = .1),
  #               alpha = .05) + 
  #   scale_y_continuous(limits=c(-5, 5))

  # # filter out tokens that introduce a lot of error in the estimated rating
  # tokVals <- tokVals %>%
  #   filter(abs(avg_err) < 1)
  # 
  # cor(tokVals$Stars, tokVals$avg_rating)
  # cor(tokVals$Stars, tokVals$wtavg_rating)

  # filter out tokens that introduce a lot of error in the estimated rating
  tokVals <- tokVals %>%
    filter(abs(avg_err) < 1)   

  # # check subsequent correlations between actual and average ratings 
  # cor3 <- cor(tokVals$Stars, tokVals$avg_rating)
  # cor4 <- cor(tokVals$Stars, tokVals$wtavg_rating)
  # 
  # plot3 <- ggplot(tokVals, aes(Stars, avg_err)) + 
  #   geom_jitter(position = position_jitter(width = .1),
  #               alpha = .05) + 
  #   scale_y_continuous(limits=c(-5, 5))
  # plot4 <- ggplot(tokVals, aes(Stars, wtavg_err)) + 
  #   geom_jitter(position = position_jitter(width = .1),
  #               alpha = .05) + 
  #   scale_y_continuous(limits=c(-5, 5))
  
  # # filter out words with a standard deviation of ratings greater than one but not 0
    # # reduces correlation between actual & mean rating
  # tokVals <- tokVals %>%
  #   filter(avg_stdev < 1) %>%
  #   filter(avg_stdev > 0)
  #
  # cor(tokVals$Stars, tokVals$avg_rating)
  # cor(tokVals$Stars, tokVals$wtavg_rating)
  
  # # filter out the lowest 25% idf (i.e., 25% most frequent terms)
    # # while messing with the standard deviation has detrimental impact on correlation,
    # # filtering out the unimportant words does not
  q <- quantile(tokVals$tf_idf, .25)
  tokVals <- tokVals %>%
    filter(idf > q)
  
  # cor(tokVals$Stars, tokVals$avg_rating)
  # cor(tokVals$Stars, tokVals$wtavg_rating)
  
  # remove duplicates
  tokVals <- tokVals %>%
    mutate(dups = duplicated(tokVals)) %>%
    filter(dups == FALSE) %>%
    select(-dups)

  # # final check on correlations
  # cor5 <- cor(tokVals$Stars, tokVals$avg_rating)
  # cor6 <- cor(tokVals$Stars, tokVals$wtavg_rating)
  # 
  # plot5 <- ggplot(tokVals, aes(Stars, avg_err)) + 
  #   geom_jitter(position = position_jitter(width = .1),
  #               alpha = .05) + 
  #   scale_y_continuous(limits=c(-5, 5))
  # plot6 <- ggplot(tokVals, aes(Stars, wtavg_err)) + 
  #   geom_jitter(position = position_jitter(width = .1),
  #               alpha = .05) + 
  #   scale_y_continuous(limits=c(-5, 5))
  # 
  # c(cor1,cor2,cor3,cor4,cor5,cor6)
  # 
  # library(cowplot)
  # plot_grid(plot1, plot2,
  #           plot3, plot4,
  #           plot5, plot6,
  #           ncol=2)
  
  return(tokVals)
  
}

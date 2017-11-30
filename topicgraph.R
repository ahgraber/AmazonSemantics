

topicgraph <- function(tbl_df, k){
  
  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, tidytext, topicmodels)

  # create td-idf by review index
  tbl_df2 <- tbl_df %>%
    count(Index, word) %>%
    bind_tf_idf(word, Index, n) %>%
    arrange(desc(tf_idf))
  
  # cast into dtm for topic model
  train_dtm <- cast_dtm(tbl_df2, Index, word, n)
  
  # set a seed so that the output of the model is predictable
  total_lda <- LDA(train_dtm, k = k, control = list(seed = 1234))
  ## A LDA_VEM topic model with 2 topics.
  
  # extracting the per-topic-per-word probabilities, called β (“beta”), from the model
  total_topics <- tidy(total_lda, matrix = "beta")
  total_topics
  
  # find top 10 terms
  total_top_terms <- total_topics %>%
    group_by(topic) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # graph top 10 terms per topic (terms are damn near equivalent???)
  # try again filtering by App
  plot1 <- total_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()  
  
  return(list(plot1))
  
}


topicgraph <- function(train_lem,k){
  
  library(topicmodels)
  # create td-idf by review index
  train_lem3 <- train_lem %>%
    count(Index, word) %>%
    bind_tf_idf(word, Index, n) %>%
    arrange(desc(tf_idf))
  
  # cast into dtm for topic model
  train_dtm <- cast_dtm(train_lem3, Index, word, n)
  
  # set a seed so that the output of the model is predictable
  total_lda <- LDA(train_dtm, k = k, control = list(seed = 1234))
  ## A LDA_VEM topic model with 2 topics.
  
  # extracting the per-topic-per-word probabilities, called β (“beta”), from the model
  total_topics <- tidy(total_lda, matrix = "beta")
  total_topics
  
  # find top 10 terms
  total_top_terms <- total_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
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
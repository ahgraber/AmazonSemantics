

topicgraph <- function(tbl_df, kk){
  
  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, tidytext, topicmodels)

  # turn tidy framework into dfm, does some add'l cleaning, adds tf_idf
  train_dfm <- tbl_df %>%
    select(-Product, -Date, -Stars) %>%
    cast_dfm(term = token, document = Index, value = n) %>%
    dfm(remove_punct = TRUE, remove_numbers = TRUE, tolower = TRUE) %>%
    dfm_trim(min_count = 4, max_docfreq = 1000, verbose = TRUE) %>%
    tfidf() 


  ldaModel <- LDA(convert(train_dfm, to = "topicmodels"), k = kk, control = list(seed=1234))
  # get_terms(ldaModel, 5)
  
  # extracting the per-topic-per-word probabilities, called β (“beta”), from the model
  total_topics <- tidy(ldaModel, matrix = "beta")
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
  
  return(list("plots" = list(plot1), "topics" = total_top_terms))
  
}
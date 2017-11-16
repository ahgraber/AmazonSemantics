#Source funtion to Parse Amazon html pages for data
# https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R


#Parse Amazon html pages for data
amazon_scraper_simple <- function(doc, delay = 0){
  
  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load_gh("trinker/sentimentr")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
  
  sec = 0
  if(delay < 0) warning("delay was less than 0: set to 0")
  if(delay > 0) sec = max(0, delay + runif(1, -1, 1))
  
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  author <- doc %>%
    html_nodes(".review-byline .author") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  # ver.purchase <- doc%>%
  #   html_nodes(".review-data.a-spacing-mini") %>%
  #   html_text() %>%
  #   grepl("Verified Purchase", .) %>%
  #   as.numeric()
  
  # format <- doc %>% 
  #   html_nodes(".review-data.a-spacing-mini") %>% 
  #   html_text() %>%
  #   gsub("Color: |\\|.*|Verified.*", "", .)
  # #if(length(format) == 0) format <- NA
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  # helpful <- doc %>%
  #   html_nodes(".cr-vote-buttons .a-color-secondary") %>%
  #   html_text() %>%
  #   str_extract("[:digit:]+|One") %>%
  #   gsub("One", "1", .) %>%
  #   as.numeric()
  
  df <- data.frame(title, author, date, stars, comments, stringsAsFactors = F)
  
  return(df)
}


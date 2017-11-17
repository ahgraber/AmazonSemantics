### Scrape Amazon for Streaming Music App reviews

#--------------------------------------------------------------------------------------------------
scrapeAmazon <- function(prod, prod_code, pages, delay) {

  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(xml2)
  
  ### Reference: https://justrthings.wordpress.com/2016/08/17/web-scraping-and-sentiment-analysis-of-amazon-reviews/
  # Add parsing function source
  if(!exists("amazon_scraper_simple.R", mode="function")) source("amazon_scraper_simple.R")
  #source("amazon_scraper_simple.R")
  
  
  # What to scrape?  *** Scraping steaming music apps - NOTE: AMAZON/ANDROID ONLY ***
  # Amazon: https://www.amazon.com/product-reviews/B004FRX0MY/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
    # prod <- "Amazon" 
    # prod_code <- "B004FRX0MY"
    # pages <- 1379
  # iHeartRadio: https://www.amazon.com/product-reviews/B005ZFOOE8/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
    # prod <- "iHeartRadio"
    # prod_code <- "B005ZFOOE8"
    # pages <- 1384
  # Pandora: https://www.amazon.com/product-reviews/B005V1N71W/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
    # prod <- "Pandora"
    # prod_code <- "B005V1N71W"
    # pages <- 1979
  # Spotify: https://www.amazon.com/product-reviews/B00KLBR6IC/ref=acr_dpappstore_text?ie=UTF8&showViewpoints=1
    # prod <- "Spotify"
    # prod_code <- "B00KLBR6IC"
    # pages <- 1504
  
  # Erase holding data frame
  reviews_all <- NULL
  
  # Loop through specified number of pages
  for(page_num in 1:pages){
    # URL of product to be reviewed (specified by prod_code)
    url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
    # entire text of given web page
    doc <- read_html(url)
    
    # use amazon_scraper script to clean and parse the different parts of the review
    reviews <- amazon_scraper_simple(doc, delay)        # contains the parsed reviews from page_num
    reviews_all <- rbind(reviews_all, cbind(prod, reviews)) # aggregated reviews from all pages
  }
  
  return(reviews_all)
}
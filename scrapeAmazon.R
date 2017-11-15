### Scrape Amazon for Streaming Music App reviews

#--------------------------------------------------------------------------------------------------
### Install packages

# Uncomment and run the first time to ensure all packages are installed
  # install.packages("tidyverse")
  # install.packages("pacman")
  # install.packages("qdap")

#--------------------------------------------------------------------------------------------------
library(pacman)
library(xml2)

### Reference: https://justrthings.wordpress.com/2016/08/17/web-scraping-and-sentiment-analysis-of-amazon-reviews/
# Source function to Parse Amazon html pages for data
# source("amazon_scraper.R")
source("amazon_scraper_simple.R")


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
  prod <- "Spotify"
  prod_code <- "B00KLBR6IC"
  pages <- 1504

# Erase holding data frame
reviews_all <- NULL

# Loop through specified number of pages
for(page_num in 1:pages){
  # URL of product to be reviewed (specified by prod_code)
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  # entire text of given web page
  doc <- read_html(url)
  
  # use amazon_scraper script to clean and parse the different parts of the review
  reviews <- amazon_scraper_simple(doc, delay = 2)        # contains the parsed reviews from page_num
  reviews_all <- rbind(reviews_all, cbind(prod, reviews)) # aggregated reviews from all pages
}

# Save as 
  # amazon.df <- reviews_all
  # write.csv(amazon.df, "Amazon.csv")
  # iheartradio.df <- reviews_all
  # write.csv(iheartradio.df, "iHeartRadio.csv")
  # pandora.df <- reviews_all
  # write.csv(pandora.df, "Pandora.csv")
  spotify.df <- reviews_all
  write.csv(spotify.df, "Spotify.csv")
  


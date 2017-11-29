library(tidyverse)
library(textclean)

# Initialize scripts
source("readin.R")
source("findTypos.R")


# Import data w/ appropriate col typing
train_df <- readin(filename="train.csv", subfolder="Scraped Data", infolder=TRUE)
colnames(train_df) <- c('Index','Product', 'Date', 'Stars','Review')
train_df$Product <- as.factor(train_df$Product)
train_df$Date <- as.Date(train_df$Date)

dummy <- sample_n(train_df)
#toclean <- dummy$Review
toclean <- train_df$Review
toclean[[1]] <- "   @4 quick,- awsome test dissapointing"

# Functions
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
nonewlines <- function(x) gsub("[\r\n]", " ", x)
nocommas <- function(x) gsub("[,]", " ", x)
nodashes <- function(x) gsub("[-]", " ", x)
noquotes <- function(x) gsub('["]', " ", x)
nobrackets <- function(x) gsub("\\[|\\]|\\{|\\}|\\(|\\)", "", x)
nospecialchar <- function(x) gsub("[@#$%^&*]", " ", x)
nosymbol <- function(x) gsub('\\p{So}|\\p{Cn}', '', x, perl = TRUE)
nomultispace <- function(x) gsub("\\s+", " ", x)

# Clean
cleaned1 <- toclean %>%
  trim() %>%
  nonewlines() %>%
  nocommas() %>%
  nodashes() %>%
  nomultispace()

cleaned2 <- cleaned1 %>%
  replace_contraction() %>%
  replace_non_ascii() %>%
  replace_symbol() %>%
  replace_ordinal() %>%
  replace_number() %>%
  replace_emoticon()
# ordinal, number, (non-ascii?) may cause problems

cleaned3 <- cleaned2 %>%
  noquotes() %>%
  nobrackets() %>%
  nospecialchar() %>%
  nosymbol() %>%
  nomultispace()
  

# fix typos
dictionary <- readin(filename="typos.csv", subfolder="Lists", infolder=TRUE)
find <- dictionary$Find
replace <- dictionary$Replace

cleaned4 <- stringi::stri_replace_all_regex(cleaned3, find, replace, vectorize_all=FALSE)

# final replace contractions inserted by typo correction
df <- replace_contraction(cleaned4)

#dummy$Review <- df
train_df$Review <- df
data.table::fwrite(train_df, file.path(paste(getwd(),"Scraped Data",sep = "/"),"dummy.csv"))

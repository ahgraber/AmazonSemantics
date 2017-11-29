fixTypos <- function (chr) {

# Package manager
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(tidyverse, stringr, tidytext, textclean)
if(!exists("readin.R", mode="function")) source("readin.R")

#--------------------------------------------------------------------------------------------------
### Fix common typos
# see findTypos.R to identify frequently misspelled words

  # typically done prior to fixing typos
  # x <- firstClean(chr)

  # use to find and replace common typos:
    # "\b" finds boundaries between words
  dictionary <- readin(filename="typos.csv", subfolder="Lists", infolder=TRUE)
  find <- dictionary$Find
  replace <- dictionary$Replace
  
  chr1 <- stringi::stri_replace_all_regex(chr, find, replace, 
                                          vectorize_all=FALSE, case_insensitive=TRUE)
    
  chr2 <- replace_contraction(chr1)
  
  return(chr2)
  
}

### check for typos - currently not working.  output is no different from input
fixTypos2 <- function() {
# see: https://www.r-bloggers.com/automatic-cleaning-of-messy-text-data/
# see: https://github.com/ahgraber/tmt/blob/master/README.md
  
  ### If using as script
  # install.packages("quanteda")
  # install.packages("devtools")
  install.packages("perl", "libtool", "gettext", "autoconf", "automake", "makeinfo")
  devtools::install_github("GNUAspell/aspell")
  # devtools::install_github("ahgraber/tmt")  # updated from schaunwheeler/tmt
  library(quanteda)
  #library(Aspell)
  library(tmt)  # note: masks "as.DocumentTermMatrix", "stopwords" from quanteda  
  #library(tidyr)
  #library(dplyr)
  
  ### NOTE: MUST INSTALL ASPELL SEPARATELY.  This is NOT an R package
  # On Windows, download aspell and dictionaries from http://aspell.net/win32/, and 
  # set the environmental PATH variable to include aspell
  
  # On a Mac, download Cocoaspell from http://people.ict.usc.edu/%7Eleuski/cocoaspell/, then go into
  # the folder where the dictionaries are kept (probably library/
  # Application/Support/cocoAspell/aspell6-en-6.0-0/) and copy all of the dictionary files. Then
  # open the Finder, click on Go>Go to Folder, type "/usr", then navigate to
  # /usr/loca/lib/aspell-0.60/ and paste them all in that directory
  
  ### If calling as function
  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(quanteda)
  #pacman::p_load_gh(GNUAspell/aspell) # not available for R?  unsure how to manage dictionary then
  pacman::p_load_gh(ahgraber/tmt)
  #pacman::p_load(tidyr)
  #pacman::p_load(dplyr)

  ### MANUAL REVIEW/FIX IS REQUIRED UNLESS WE CAN TWEAK THE DICTIONARY TO IMPROVE RELIABILITY
    # Based on review, we either need to improve dictionary to update for 2017-era internet 
    # terminology, or we can just continue without spelling fixes.  This will decrease reliability 
    # a little bit (we'll miss spelling variants of "Spotify", etc) but getting reliable spelling 
    # fixes is probably not worth the amount of time required to fix
  # AND
    # the majority of the common misspellings we can probably accomodate via synonym identification
  
  # update dictionary: http://www.omegahat.net/Aspell/aspell.html
  # create spelling object to update current session
  sp = getSpeller()
  
  # add valid spellings to current session
  addToList(c("omegahat", "SJava"), sp)
  
  # add corrections/recommended spelling
  addCorrection(sp, duncn = "duncan",  ro = "rho", statistcs = "statistics")
  
  # gather data to be spellchecked
  temp <- as.character(tokens(training.df$Review))
  
  eval <- aspellCheck(temp,
                      output="eval",
                      sep=FALSE,
                      ignore=NULL,  ## could add valid words to ignore list (i.e., Spotify)
                      split.missing = TRUE)
  
  fixed <- aspellCheck(temp,
                       output = "fix", 
                       sep = FALSE, 
                       cap.flag = "none", 
                       ignore=NULL, 
                       split.missing = TRUE)
   
  
  # compare fixed with temp (correct spellings are NA in 'fixed')
  compareSpell <- cbind.data.frame(temp,eval,fixed)
  write.csv(compareSpell, file.path(paste(getwd(),"Scraped Data",sep = "/"),"compareSpell.csv"))
  

  
# AspellCheck() takes as input a single character string. 
# The output has three modes:
  # * "eval" returns a logical vector indicating whether each word was found in the dictionary. 
  # * "sugg" returns a list where each misspelled word is given all suggested
  # alternatives, and where each correctly spelled word is given NA. 
  # * "fix" replaces each misspelled word with the word suggested as its most likely alternative. 
  # Proper nouns are not considered viable alternatives. 
# The 'sep' option take a logical value and specifies whether two separate words should be 
  # considered a viable alternative to a misspelled word. 
# 'cap.flag' takes one of three values: 
  # 'none' (the default) does nothing; 
  # 'first' tells the function to ignore all words that start with a capital letter; 
  # "all" tells the function to ignore all words that are entirely composed of capital letters. 
  # This allows subject-specific words to survive the spell check. 
# The 'ignore' option takes a character vector and, like cap.flag, give the function a list of words 
  # to ignore. 
#'split.missing' takes a logical value. When set to TRUE, it makes a call to SplitWords() function 
  # in this package in each case where a word is not found in the dictionary and a viable
  # alternative cannot be found.

}

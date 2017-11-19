### check for typos - currently not working.  output is no different from input
fixTypos2 <- function() {
# see: https://www.r-bloggers.com/automatic-cleaning-of-messy-text-data/
# see: https://github.com/ahgraber/tmt/blob/master/README.md
  
  ### If using as script
  # install.packages("devtools")
  # devtools::install_github("ahgraber/tmt")  # updated from schaunwheeler/tmt
  library(tmt)  # note: masks "as.DocumentTermMatrix", "stopwords" from quanteda  
  
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
  pacman::p_load_gh(ahgraber/tmt)

  temp <- training.df$Review[1]
  
  aspellCheck(temp, 
              output = "fix", 
              sep = FALSE, 
              cap.flag = "none", 
              ignore=NULL, 
              split.missing = TRUE)

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

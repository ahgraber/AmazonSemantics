### Remove contractions

removeContractions <- function(df) {
  
  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(textclean)
  
  # Find contractions, replace with distinct words
  replace_
  
  return(df) 
}
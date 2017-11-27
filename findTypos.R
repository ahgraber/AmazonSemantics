findTypos <- function (x) {### check for typos 

# Package manager
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(tidyverse, textclean, hunspell)
if(!exists("firstClean.R", mode="function")) source("firstClean.R")

# library(tidyverse)
# library(textclean)
# library(hunspell)
# # library(quanteda)
# source("firstClean.R")

#--------------------------------------------------------------------------------------------------

text <- mapply(replace_contraction, toclean_df)
typolist <- hunspell(text)
typolist <- as_data_frame(unlist(typolist, recursive=T)) %>%
  count(value, sort = T) 

return(typolist)

}
###currently not working.  output is no different from input
# conflict between hunspell & quanteda re: dictionary
# detach(package:quanteda)
# detach(package:hunspell)  
# library(quanteda)
# 
# # frequently occurring typos to fix:
# typo.dict <- dictionary(list(awesome = "awsome",
#                   favorite = c("favourite", "favs", "fav"),
#                   "every time" = "everytime",
#                   recommend = c("reccomend","recomend"),
#                   commercials = "comercials",
#                   disappointed = "dissapointed",
#                   really = "realy",
#                   thanks = "thx",
#                   love = "luv",
#                   because = c("bc","cuz"),
#                   excellent = c("excelent","excelente"),
#                   their = "thier",
#                   minutes = "mins",
#                   lot = "alot",
#                   little = "lil",
#                   pandora = "panadora",
#                   spotify = "spodify",
#                   doesnt = "dosent",
#                   soregex = "so*o"))
# 
# # checking typo frequencies after dictionary applied
# fixedTypos <- dfm_lookup(typos, typo.dict, exclusive = F,
#                          valuetype = "regex")
    ### would "tokens_replace() work instead of dfm_lookup()?
# fixedTypos <- t(as.data.frame(fixedTypos))
# 
# library(hunspell)
# checkTypos <- hunspell(as.character(fixedTypos))
# detach(package:hunspell)
# library(quanteda)
# 
# checkTypos <- unlist(typos, recursive=T)
# checkTypos <- dfm(checkTypos)
# topfeatures(checkTypos, 200)
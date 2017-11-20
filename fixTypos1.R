### check for typos - currently not working.  output is no different from input
# library(hunspell)
# library(dplyr)
# 
# reviews <- training.df$Review
# typos <- hunspell(reviews)
# typos <- unlist(typos, recursive=T)
# typos <- dfm(typos)
# 
# topfeatures(typos, 200)
# detach(package:hunspell)  # conflict between hunspell & quanteda re: dictionary
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
#                   listen = "listen",
#                   pandora = "panadora",
#                   spotify = "spodify",
#                   doesnt = "dosent",
#                   soregex = "so*o"))
# 
# # checking typo frequencies after dictionary applied
# fixedTypos <- dfm_lookup(typos, typo.dict, exclusive = F,
#                          valuetype = "regex")
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
fixTypos <- function (x) {

# Package manager
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(tidyverse, tidytext)
if(!exists("firstClean.R", mode="function")) source("firstClean.R")

#--------------------------------------------------------------------------------------------------
### Fix common typos
# see findTypos.R to identify frequently misspelled words

  y <- firstClean(x)
  # create "typos" and "Replacements" list from wordListMgmt.R
  # use to find and replace common typos
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("awsome", ignore_case=T), "awesome")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("favourite", ignore_case=T), "favorite")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("favs", ignore_case=T), "favorite")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("fav", ignore_case=T), "favorite")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("reccomend", ignore_case=T), "recommend")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("recomend", ignore_case=T), "recommend")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("reccommend", ignore_case=T), "recommend")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("comercials", ignore_case=T), "commercials")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("dissapointed", ignore_case=T), "disappointed")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("realy", ignore_case=T), "really")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("thx", ignore_case=T), "thanks")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("luv", ignore_case=T), "love")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("bc", ignore_case=T), "because")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("cuz", ignore_case=T), "because")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("excelent", ignore_case=T), "excellent")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("excelente", ignore_case=T), "excellent")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("thier", ignore_case=T), "their")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("mins", ignore_case=T), "minutes")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("alot", ignore_case=T), "a lot")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("lil", ignore_case=T), "little")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("panadora", ignore_case=T), "pandora")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("spodify", ignore_case=T), "spotify")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("dosent", ignore_case=T), "does not")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("everytime", ignore_case=T), "every time")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("ive", ignore_case=T), "I have")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("Ive", ignore_case=T), "I have")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("im", ignore_case=T), "I am")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("ur", ignore_case=T), "your")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("th", ignore_case=T), "the")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("ap", ignore_case=T), "app")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("programing", ignore_case=T), "programming")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("dont", ignore_case=T), "do not")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("havent", ignore_case=T), "have not")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("couldnt", ignore_case=T), "could not")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("varity", ignore_case=T), "variety")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("ot", ignore_case=T), "to")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("theres", ignore_case=T), "there is")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("definately", ignore_case=T), "definitely")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("definetly", ignore_case=T), "definitely")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("raido", ignore_case=T), "radio")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("isnt", ignore_case=T), "is not")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("lisen", ignore_case=T), "listen")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("ppl", ignore_case=T), "people")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("similiar", ignore_case=T), "similar")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("travelling", ignore_case=T), "traveling")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("acount", ignore_case=T), "account")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("diffrent", ignore_case=T), "different")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("plz", ignore_case=T), "please")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("truely", ignore_case=T), "truly")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("Sirrius", ignore_case=T), "Sirius")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("totaly", ignore_case=T), "totally")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("tryed", ignore_case=T), "tried")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("wasnt", ignore_case=T), "was not")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("arent", ignore_case=T), "are not")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("aps", ignore_case=T), "apps")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("stoped", ignore_case=T), "stopped")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("synch", ignore_case=T), "sync")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("iheart radio", ignore_case=T), "iheartradio")})
  y <- sapply(y, FUN= function(y) {str_replace_all(y, coll("wi fi", ignore_case=T), "wifi")})
  
  # these remove everything containing "so*o" or "uh*h", not just those terms
  y <- sapply(y, FUN = function(y) {str_replace(y, regex("\bso*o\b", ignore_case=T), "so")})
  y <- sapply(y, FUN = function(y) {str_replace(y, regex("\buh*h\b", ignore_case=T), "uh")})
  


  return(y)
}



  # typo <- c("awsome", "favourite", "favs", "fav", "reccomend", 
  #           "recomend", "comercials", "dissapointed", "realy", "thx", 
  #           "luv", "bc", "cuz", "excelent", "excelente",
  #           "thier", "mins", "alot", "lil", "panadora", 
  #           "spodify", "dosent", "ive", "Ive", "everytime" )
  # replacement <- c("awesome", "favorite", "favorite", "favorite", "recommend",
  #                  "recommend", "commercials", "disappointed", "really", "thanks",
  #                  "love", "because", "because", "excellent", "excellent",
  #                  "their", "minutes", "a lot", "little", "pandora",
  #                  "spotify", "does not", "I have", "I have", "every time")
  # 
  # so_regex = "/bso*o/b"
  # uh_regex = "/buh*h/b
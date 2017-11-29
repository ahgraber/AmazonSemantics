fixTypos <- function (x) {

# Package manager
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(tidyverse, stringr, tidytext)
if(!exists("firstClean.R", mode="function")) source("firstClean.R")

#--------------------------------------------------------------------------------------------------
### Fix common typos
# see findTypos.R to identify frequently misspelled words

  x <- firstClean(x)
  # create "typos" and "Replacements" list from wordListMgmt.R
  # use to find and replace common typos:
    # "\b" finds boundaries between words; to escape the first "\" we have to double it
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bawsome\\b", ignore_case=T), "awesome")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bfavourite\\b", ignore_case=T), "favorite")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bfavs\\b", ignore_case=T), "favorite")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bfav\\b", ignore_case=T), "favorite")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\breccomend\\b", ignore_case=T), "recommend")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\brecomend\\b", ignore_case=T), "recommend")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\breccommend\\b", ignore_case=T), "recommend")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bcomercials\\b", ignore_case=T), "commercials")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bdissapointed\\b", ignore_case=T), "disappointed")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\brealy\\b", ignore_case=T), "really")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bthx\\b", ignore_case=T), "thanks")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bluv\\b", ignore_case=T), "love")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bbc\\b", ignore_case=T), "because")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bcuz\\b", ignore_case=T), "because")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bexcelent\\b", ignore_case=T), "excellent")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bexcelente\\b", ignore_case=T), "excellent")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bthier\\b", ignore_case=T), "their")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bmins\\b", ignore_case=T), "minutes")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\balot\\b", ignore_case=T), "a lot")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\blil\\b", ignore_case=T), "little")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bpanadora\\b", ignore_case=T), "pandora")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bspodify\\b", ignore_case=T), "spotify")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\beverytime\\b", ignore_case=T), "every time")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bur\\b", ignore_case=T), "your")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bth\\b", ignore_case=T), "the")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bap\\b", ignore_case=T), "app")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bprograming\\b", ignore_case=T), "programming")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bdosent\\b", ignore_case=T), "doesn't")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bive\\b", ignore_case=T), "I've")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bIve\\b", ignore_case=T), "I've")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bim\\b", ignore_case=T), "I'm")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bdont\\b", ignore_case=T), "don't")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bhavent\\b", ignore_case=T), "haven't")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bcouldnt\\b", ignore_case=T), "couldn't")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bisnt\\b", ignore_case=T), "isn't")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bwasnt\\b", ignore_case=T), "wasn't")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\barent\\b", ignore_case=T), "aren't")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\btheres\\b", ignore_case=T), "there's")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bvarity\\b", ignore_case=T), "variety")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bot\\b", ignore_case=T), "to")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bdefinately\\b", ignore_case=T), "definitely")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bdefinetly\\b", ignore_case=T), "definitely")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\braido\\b", ignore_case=T), "radio")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\blisen\\b", ignore_case=T), "listen")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bppl\\b", ignore_case=T), "people")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bsimiliar\\b", ignore_case=T), "similar")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\btravelling\\b", ignore_case=T), "traveling")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bacount\\b", ignore_case=T), "account")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bdiffrent\\b", ignore_case=T), "different")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bplz\\b", ignore_case=T), "please")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\btruely\\b", ignore_case=T), "truly")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bSirrius\\b", ignore_case=T), "Sirius")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\btotaly\\b", ignore_case=T), "totally")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\btryed\\b", ignore_case=T), "tried")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\baps\\b", ignore_case=T), "apps")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bstoped\\b", ignore_case=T), "stopped")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("\\bsynch\\b", ignore_case=T), "sync")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("iheart radio", ignore_case=T), "iheartradio")})
  x <- sapply(x, FUN= function(y) {str_replace_all(y, regex("wi fi", ignore_case=T), "wifi")})
  
  # these remove everything containing "so*o" or "uh*h", not just those terms
  x <- sapply(x, FUN = function(y) {str_replace(y, regex("\\bso*o\\b", ignore_case=T), "so")})
  x <- sapply(x, FUN = function(y) {str_replace(y, regex("\\buh*h\\b", ignore_case=T), "uh")})
  


  return(x)
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
  # so_regex = "\\bso*o\\b"
  # uh_regex = "\\buh*h/b
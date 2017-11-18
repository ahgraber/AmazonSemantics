#CORPUS

#install.package("quanteda")

#library calls
library(quanteda)
library(stringr)
library(NLP)
library(tm)

#creating corpus split at every space
textbag <- str_split(amazon.df$review,pattern = "\\s+") #can also use tokens(amazon.df$review)
#from list to character
textbag <- unlist(textbag)

#get positive and negative words
poswords <- scan("D:/Coursework syllabus/quarter 1/MIS 612/Class project/Github/MIS612/poswords.txt", what = 'character', comment.char = ';') #edit to pref
negwords <- scan("D:/Coursework syllabus/quarter 1/MIS 612/Class project/Github/MIS612/negwords.txt", what = 'character', comment.char = ';') #edit to pref

#get number of positive words for each review
pscore <- unlist(lapply(textbag,function(x){
  sum(!is.na(match(x,poswords)))
}))
sum(pscore) #positive word score

#get number of negative words for each review
nscore <- unlist(lapply(textbag,function(x){
  sum(!is.na(match(x,negwords)))
}))
sum(nscore) #negative word score

#Top features in the textbag
mydfm <- dfm(textbag)
topfeatures(mydfm,20) #frequency of top 20 words

#create a score = no of poswords - no of negwords (in textbag)
score = sum(!is.na(match(textbag,poswords))) - sum(!is.na(match(textbag,negwords)))

#create a wordcloud with min frequency
textplot_wordcloud(mydfm, min.freq = 150)
textplot_wordcloud(mydfm, min.freq = 150, random.order = FALSE, colors = rainbow(3))

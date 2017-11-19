#CORPUS

#install.package("quanteda")

#library calls
library(quanteda)
library(stringr)
library(NLP)
library(tm)
library(RCurl)
library(syuzhet)
library(ggplot2)
#readcsv
amazon.df <- read.csv("D:/Coursework syllabus/quarter 1/MIS 612/Class project/Github/MIS612/Scraped Data/Amazon_firstcleaned.csv")
amazon.df$comments <- as.character(amazon.df$comments)

#creating corpus split at every space
textbag <- corpus(amazon.df$comments)

#tokenizing
mytok <- tokens(textbag ,remove_numbers = TRUE,  remove_punct = TRUE,remove_separators = TRUE)
mytok <- gsub(pattern = "\\b[A-z]\\b{1}", replacement = " ",mytok) #replace flying letters

#sentiment analysis
mysentiment <- get_nrc_sentiment(mytok)
sentimentscores <- data.frame(colSums(mysentiment[,]))
names(sentimentscores) <- "score"
sentimentscores <- cbind("sentiment" = rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data = sentimentscores,aes(x= sentiment, y = score)) + 
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("score") + ggtitle ("Total sentiment score based on reviews")

#remove stopwords
mydata <- dfm(mytok, remove = stopwords("english"), stem = TRUE)
mydata <- dfm(mydata, remove = c("amazon","app","music")) #remove other common words

#Top features in the textbag
topfeatures(mydata,20) #frequency of top 20 words

#get positive and negative words
poswords <- scan("D:/Coursework syllabus/quarter 1/MIS 612/Class project/Github/MIS612/poswords.txt", what = 'character', comment.char = ';') #edit to pref
negwords <- scan("D:/Coursework syllabus/quarter 1/MIS 612/Class project/Github/MIS612/negwords.txt", what = 'character', comment.char = ';') #edit to pref

#get number of positive words for each review
pscore <- unlist(lapply(mytok,function(x){
  sum(!is.na(match(x,poswords)))
}))
sum(pscore) #positive word score

#get number of negative words for each review
nscore <- unlist(lapply(mytok,function(x){
  sum(!is.na(match(x,negwords)))
}))
sum(nscore) #negative word score


#create a score = no of poswords - no of negwords (in textbag)
score = sum(!is.na(match(mytok,poswords))) - sum(!is.na(match(mytok,negwords)))

#create a wordcloud with min frequency
textplot_wordcloud(mydata, min.freq = 150)
textplot_wordcloud(mydata, min.freq = 150, random.order = FALSE ,colors = rainbow(3))

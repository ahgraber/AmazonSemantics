### Function to save & import stopwords list(s) for manual editing

# wordListMgmt 

#--------------------------------------------------------------------------------------------------

source(readin.R)
dataPath <- paste(getwd(),"Lists",sep="/")

#--------------------------------------------------------------------------------------------------
### Save list(s) for review/revision

## Stopwords
  # save SMART stopword list for editing
  SMART.list <- as.data.frame(data_char_stopwords$SMART)
  write.table(SMART.list, file = paste(dataPath, "SMART_stop.txt", sep = "/"))
  
  # save english stopword list for editing
  en.list <- as.data.frame(data_char_stopwords$english)
  write.table(SMART.list, file = paste(dataPath, "en_stop.txt", sep = "/"))

## Lemmas
  # save lemma list for editing (from http://www.lexiconista.com/Datasets/lemmatization-en.zip)
  temp <- tempfile()
  download.file("http://www.lexiconista.com/Datasets/lemmatization-en.zip",temp)
  lemma.list <- read.table(unz(temp, "lemmatization-en.txt"),sep="",quote="")
  colnames(lemma.list) <- c("Lemma","Source")
  write.table(lemma.list, file = paste(dataPath, "lemmas.txt", sep = "/"))
  unlink(temp)
  
## Synonyms


#--------------------------------------------------------------------------------------------------
### Manually edit lists

## Stopwords notes:
  
## Lemmas notes:
  
## Synonyms notes:
  typo.dict <- dictionary(list(awesome = "awsome",
                               favorite = c("favourite", "favs", "fav"),
                               "every time" = "everytime",
                               recommend = c("reccomend","recomend"),
                               commercials = "comercials",
                               disappointed = "dissapointed",
                               really = "realy",
                               thanks = "thx",
                               love = "luv",
                               because = c("bc","cuz"),
                               excellent = c("excelent","excelente"),
                               their = "thier",
                               minutes = "mins",
                               lot = "alot",
                               listen = "listen",
                               pandora = "panadora",
                               spotify = "spodify",
                               doesnt = "dosent",
                               soregex = "so*o"))
#--------------------------------------------------------------------------------------------------
### Import edited lists

## Stopwords
  SMART.list <- readin("SMART_stop.txt", folder="Lists", infolder=T)
  en.list <- readin("en_stop.txt", folder="Lists", infolder=T)
  stopwords.df <- as.data.frame(c(SMART.list, en.list))

## Lemmas
  lemma.list <- readin("lemmas.txt", folder="Lists", infolder=T)
    
## Synonyms
  

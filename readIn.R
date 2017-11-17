# Duplicate data
readin <- function(x) {
  read.csv(x,
           colClasses=c('NULL','character','character','character','character','numeric','character'), 
           stringsAsFactors = FALSE)
}

# Assuming you have a folder named "Scraped Data" where the data provided for the project lives...
dataPath <- paste(getwd(),"/Scraped Data",sep="") 

amazon.df <- readin(file.path(dataPath,"Amazon.csv"))
iheartradio.df <- readin(file.path(dataPath,"iHeartRadio.csv"))
pandora.df <- readin(file.path(dataPath,"Pandora.csv"))
spotify.df <- readin(file.path(dataPath,"Spotify.csv"))

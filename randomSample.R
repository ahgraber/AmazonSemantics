randomSample <- function(x, y) {
  df <- x
  
  # vector of booleans to pick which rows to keep in the new data frame
  booleans <- sample(1:10, nrow(df), replace = T)<=y/10
  
  # newdf has the restricted sample
  newdf <- df[booleans,]
  testdf <- df[!booleans,]
  
  
  # counters % of total
  rows <- nrow(df)
  newrows <- nrow(newdf)
  
  # counters for each service in original data set
  spotify <- sum(df$Product=="Spotify", na.rm=TRUE)
  amazon <- sum(df$Product=="Amazon", na.rm=TRUE)
  pandora <- sum(df$Product=="Pandora", na.rm=TRUE)
  iheartradio <- sum(df$Product=="iHeartRadio", na.rm=TRUE)
  
  # counters for each service in randomly selected data set
  newiheartradio <- sum(newdf$Product=="iHeartRadio", na.rm=TRUE)
  newpandora <- sum(newdf$Product=="Pandora", na.rm=TRUE)
  newamazon <- sum(newdf$Product=="Amazon", na.rm=TRUE)
  newspotify <- sum(newdf$Product=="Spotify", na.rm=TRUE)
  
  # counters for each service in original data set
  onestar <- sum(df$Stars==1, na.rm=TRUE)
  twostar <- sum(df$Stars==2, na.rm=TRUE)
  threestar <- sum(df$Stars==3, na.rm=TRUE)
  fourstar <- sum(df$Stars==4, na.rm=TRUE)
  fivestar <- sum(df$Stars==5, na.rm=TRUE)
  
  # counters for each rating in randomly selected data set
  newonestar <- sum(newdf$Stars==1, na.rm=TRUE)
  newtwostar <- sum(newdf$Stars==2, na.rm=TRUE)
  newthreestar <- sum(newdf$Stars==3, na.rm=TRUE)
  newfourstar <- sum(newdf$Stars==4, na.rm=TRUE)
  newfivestar <- sum(newdf$Stars==5, na.rm=TRUE)
  
  # names and data entries for data frame
  names <- c("Total%", "Spotify%", "Amazon%", "Pandora%", "iHeartRadio%", 
             "OneStar%", "TwoStar%", "ThreeStar%", "FourStar%", "FiveStar%")
  percents <- c(newrows/rows*100, newspotify/spotify*100, newamazon/amazon*100, newpandora/pandora*100, 
                newiheartradio/iheartradio*100, newonestar/onestar*100, newtwostar/twostar*100, 
                newthreestar/threestar*100, newfourstar/fourstar*100, newfivestar/fivestar*100)
  deviations <- percents - newrows/rows*100
  
  # data frame with metrics on whether or not the sample is representative
  checkdf <- data.frame(row.names = names, percents, deviations)
  print(checkdf)
  
  return(list(newdf,testdf))
}
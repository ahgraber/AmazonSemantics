### Read scraped data and run initial clean

#--------------------------------------------------------------------------------------------------
### Install packages

# Uncomment and run the first time to ensure all packages are installed
  # install.packages("qdap")

#--------------------------------------------------------------------------------------------------
### Data Cleaning
library(qdap)

# Create cleaned .csv files ready for multiple different analyses

# Duplicate data
readin <- function(x) {
  read.csv(x,
           colClasses=c('NULL','character','character','character','character','numeric','character'), 
           stringsAsFactors = FALSE)
}

# Assuming you have a folder named "Scraped Data" where the data provided for the project lives...
dataPath <- paste(getwd(),"/Scraped Data",sep="") 

c.amazon.df <- readin(file.path(dataPath,"Amazon.csv"))
c.iheartradio.df <- readin(file.path(dataPath,"iHeartRadio.csv"))
c.pandora.df <- readin(file.path(dataPath,"Pandora.csv"))
c.spotify.df <- readin(file.path(dataPath,"Spotify.csv"))

data.list <- list(c.amazon.df,c.iheartradio.df,c.pandora.df,c.spotify.df)

#--------------------------------------------------------------------------------------------------
### Aggregate list of cleaning functions 
  # Note: Keeping at a relatively granular level so we can control exactly what we're cleaning

# Function to remove leading and trailing whitespace
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# Replace carriage returns with space 
  # use caution, this can break format and collapse all rows into single row
nonewlines <- function(x) gsub("[\r\n]", " ", x)

# Replaces commas, periods, exclamations with space
nocommas <- function(x) gsub("[,.!]", " ", x)

# Replaces dashes with a space
nodashes <- function(x) gsub("[-]", " ", x)

# Replaces ' and " with a space
noapostrophe <- function(x) gsub("[']", " ", x)
noquotes <- function(x) gsub('["]', " ", x)

# Remove []{}()
nobrackets <- function(x) rm_bracket(x, pattern = "all", trim = FALSE, clean = FALSE,
                                     replacement = "",extract = FALSE)

# Replaces other punctionation @#$%^&* with a space
nospecialchar <- function(x) gsub("[@#$%^&*]", " ", x)

# Remove multiple spaces in strings
nomultispace <- function(x) gsub("\\s+", " ", x)

# Convert all upper case to lower case
# use 'tolower(x)'

#--------------------------------------------------------------------------------------------------
# Take data and run through cleaning process

# There MUST be a smarter way to do this quickly, but I can't figure it out right now, so... 
# brute force it is


# Apply cleaning functions to each column of the data frame
  df <- mapply(trim, df)
  df <- mapply(nonewlines, df)
  df <- mapply(nocommas, df)
  df <- mapply(nodashes, df)
  df <- mapply(noapostrophe, df)
  df <- mapply(noquotes, df)
  df <- mapply(nobrackets, df)
  df <- mapply(nospecialchar, df)
  df <- mapply(nomultispace, df)
  df <- mapply(tolower, df)
  



  

# Amazon
  c.amazon.df$title <- trim(c.amazon.df$title)
  c.amazon.df$title <- nonewlines(c.amazon.df$title)
  c.amazon.df$title <- nocommas(c.amazon.df$title)
  c.amazon.df$title <- nodashes(c.amazon.df$title)
  c.amazon.df$title <- noapostrophe(c.amazon.df$title)
  c.amazon.df$title <- noquotes(c.amazon.df$title)
  c.amazon.df$title <- nobrackets(c.amazon.df$title)
  c.amazon.df$title <- nospecialchar(c.amazon.df$title)
  c.amazon.df$title <- nomultispace(c.amazon.df$title)
  c.amazon.df$title <- tolower(c.amazon.df$title)

  c.amazon.df$comments <- trim(c.amazon.df$comments)
  c.amazon.df$comments <- nonewlines(c.amazon.df$comments)
  c.amazon.df$comments <- nocommas(c.amazon.df$comments)
  c.amazon.df$comments <- nodashes(c.amazon.df$comments)
  c.amazon.df$comments <- noapostrophe(c.amazon.df$comments)
  c.amazon.df$comments <- noquotes(c.amazon.df$comments)
  c.amazon.df$comments <- nobrackets(c.amazon.df$comments)
  c.amazon.df$comments <- nospecialchar(c.amazon.df$comments)
  c.amazon.df$comments <- nomultispace(c.amazon.df$comments)
  c.amazon.df$comments <- tolower(c.amazon.df$comments)

# iHeartRadio
  c.iheartradio.df$title <- trim(c.iheartradio.df$title)
  c.iheartradio.df$title <- nonewlines(c.iheartradio.df$title)
  c.iheartradio.df$title <- nocommas(c.iheartradio.df$title)
  c.iheartradio.df$title <- nodashes(c.iheartradio.df$title)
  c.iheartradio.df$title <- noapostrophe(c.iheartradio.df$title)
  c.iheartradio.df$title <- noquotes(c.iheartradio.df$title)
  c.iheartradio.df$title <- nobrackets(c.iheartradio.df$title)
  c.iheartradio.df$title <- nospecialchar(c.iheartradio.df$title)
  c.iheartradio.df$title <- nomultispace(c.iheartradio.df$title)
  c.iheartradio.df$title <- tolower(c.iheartradio.df$title)
  
  c.iheartradio.df$comments <- trim(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- nonewlines(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- nocommas(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- nodashes(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- noapostrophe(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- noquotes(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- nobrackets(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- nospecialchar(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- nomultispace(c.iheartradio.df$comments)
  c.iheartradio.df$comments <- tolower(c.iheartradio.df$comments)

# Pandora
  c.pandora.df$title <- trim(c.pandora.df$title)
  c.pandora.df$title <- nonewlines(c.pandora.df$title)
  c.pandora.df$title <- nocommas(c.pandora.df$title)
  c.pandora.df$title <- nodashes(c.pandora.df$title)
  c.pandora.df$title <- noapostrophe(c.pandora.df$title)
  c.pandora.df$title <- noquotes(c.pandora.df$title)
  c.pandora.df$title <- nobrackets(c.pandora.df$title)
  c.pandora.df$title <- nospecialchar(c.pandora.df$title)
  c.pandora.df$title <- nomultispace(c.pandora.df$title)
  c.pandora.df$title <- tolower(c.pandora.df$title)
  
  c.pandora.df$comments <- trim(c.pandora.df$comments)
  c.pandora.df$comments <- nonewlines(c.pandora.df$comments)
  c.pandora.df$comments <- nocommas(c.pandora.df$comments)
  c.pandora.df$comments <- nodashes(c.pandora.df$comments)
  c.pandora.df$comments <- noapostrophe(c.pandora.df$comments)
  c.pandora.df$comments <- noquotes(c.pandora.df$comments)
  c.pandora.df$comments <- nobrackets(c.pandora.df$comments)
  c.pandora.df$comments <- nospecialchar(c.pandora.df$comments)
  c.pandora.df$comments <- nomultispace(c.pandora.df$comments)
  c.pandora.df$comments <- tolower(c.pandora.df$comments)    

# Spotify
  c.spotify.df$title <- trim(c.spotify.df$title)
  c.spotify.df$title <- nonewlines(c.spotify.df$title)
  c.spotify.df$title <- nocommas(c.spotify.df$title)
  c.spotify.df$title <- nodashes(c.spotify.df$title)
  c.spotify.df$title <- noapostrophe(c.spotify.df$title)
  c.spotify.df$title <- noquotes(c.spotify.df$title)
  c.spotify.df$title <- nobrackets(c.spotify.df$title)
  c.spotify.df$title <- nospecialchar(c.spotify.df$title)
  c.spotify.df$title <- nomultispace(c.spotify.df$title)
  c.spotify.df$title <- tolower(c.spotify.df$title)
  
  c.spotify.df$comments <- trim(c.spotify.df$comments)
  c.spotify.df$comments <- nonewlines(c.spotify.df$comments)
  c.spotify.df$comments <- nocommas(c.spotify.df$comments)
  c.spotify.df$comments <- nodashes(c.spotify.df$comments)
  c.spotify.df$comments <- noapostrophe(c.spotify.df$comments)
  c.spotify.df$comments <- noquotes(c.spotify.df$comments)
  c.spotify.df$comments <- nobrackets(c.spotify.df$comments)
  c.spotify.df$comments <- nospecialchar(c.spotify.df$comments)
  c.spotify.df$comments <- nomultispace(c.spotify.df$comments)
  c.spotify.df$comments <- tolower(c.spotify.df$comments)
  
#--------------------------------------------------------------------------------------------------
# Save cleaned data

  
# Assuming you have a folder named "Scraped Data" where the data provided for the project lives...
dataPath <- paste(getwd(),"/Scraped Data",sep="") 

write.csv(c.amazon.df, (file.path(dataPath,"Amazon_firstcleaned.csv")))
write.csv(c.iheartradio.df, (file.path(dataPath,"iHeartRadio_firstcleaned.csv")))
write.csv(c.pandora.df, (file.path(dataPath,"Pandora_firstcleaned.csv")))
write.csv(c.spotify.df, (file.path(dataPath,"Spotify_firstcleaned.csv")))


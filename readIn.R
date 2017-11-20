# Duplicate data
readin <- function(filename, folder="", infolder=FALSE) {
  
  # Pass in file name, folder (optional), and whether to search for a folder for the file (optional)
  
  if (infolder) {
    # if the file is in a folder, create the folder path
    dataPath <- paste(getwd(),folder,sep="/") 
    
  } else {
    # othewise file is in the current working directory
    dataPath <- getwd()    
    
  }

  
  df <- read.csv(file.path(dataPath,filename), stringsAsFactors = FALSE)
  df <- df[,-1]
  
  return(as.data.frame(df))
}



# Duplicate data
readin <- function(x) {
  df <- read.csv(x, stringsAsFactors = FALSE)
  df <- df[,-1]
  
  return(as.data.frame(df))
}



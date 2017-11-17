# Duplicate data
readin <- function(x) {
  read.csv(x,
           colClasses=c('NULL','character','character','character','character','numeric','character'), 
           stringsAsFactors = FALSE)
}



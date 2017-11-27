### Functions and transformation for initial clean

#--------------------------------------------------------------------------------------------------
firstClean <- function(df) {

  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(tidyverse, textclean)
  
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
  nobrackets <- function(x) gsub("\\[|\\]|\\{|\\}|\\(|\\)", "", x)
  
  # Replaces other punctionation @#$%^&* with a space
  nospecialchar <- function(x) gsub("[@#$%^&*]", " ", x)
  
  # Remove multiple spaces in strings
  nomultispace <- function(x) gsub("\\s+", " ", x)
  
  # Convert all upper case to lower case
  # use 'tolower(x)'
  
  #--------------------------------------------------------------------------------------------------

  # report potential issues pre-clean - NOTE: TIME INTENSIVE, USE WITH SMALL DATA
  # check_text(df)
  
  # Take data and run through cleaning process
  
  # textclean functions!!
    df <- mapply(replace_contraction,as_data_frame(df))
    df <- mapply(replace_symbol,as_data_frame(df))
    #df <- mapply(replace_ordinal,as_data_frame(df))
    df <- mapply(replace_number,as_data_frame(df))
    df <- mapply(replace_emoticon,as_data_frame(df))
    #df <- mapply(replace_non_ascii,as_data_frame(df))
  
  # Apply other cleaning functions to each column of the data frame
    df <- mapply(trim, as_data_frame(df))
    df <- mapply(nonewlines, as_data_frame(df))
    #df <- mapply(nocommas, as_data_frame(df))
    #df <- mapply(nodashes, as_data_frame(df))
    #df <- mapply(noapostrophe, as_data_frame(df))
    df <- mapply(noquotes, as_data_frame(df))
    df <- mapply(nobrackets, as_data_frame(df))
    #df <- mapply(nospecialchar, as_data_frame(df))
    df <- mapply(nomultispace, as_data_frame(df))
    #df <- mapply(tolower, as_data_frame(df))
    

    # check text post-clean - NOTE: TIME INTENSIVE, USE WITH SMALL DATA
    # check_text(df)
    
    return(as_data_frame(df))
}


complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  library(stringr)
  complete_stats <- data.frame(id = numeric() , nobs = numeric() )
  doc_count <- 1
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  for (i in id){
    filename <- str_pad(i, 3, pad="0")
    filepath <- paste(directory[[1]], "/", filename, ".csv", sep="")
    cur_file <- read.csv( file = filepath, head = T, sep=",")
    cur_complete <- cur_file[complete.cases(cur_file),]
    
    complete_stats[doc_count,] <- c(i, nrow(cur_complete))
    doc_count <- doc_count + 1
    
  }
 
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  return(complete_stats)
}
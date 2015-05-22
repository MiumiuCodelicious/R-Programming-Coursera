pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  library(stringr)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  mean <- 0
  divisor <- 0
  for (i in id){
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    filename <- str_pad(i, 3, pad="0")
    filepath <- paste(directory[[1]], "/", filename, ".csv", sep="")

    cur_file <- read.csv( file = filepath, head = T, sep=",")
    pollutant_col <- na.omit( subset(cur_file, select=pollutant) )
    divisor <- divisor + nrow(pollutant_col)
    mean <- mean + colSums(pollutant_col, na.rm=T)

  }
  
  return( signif(mean/divisor, digits=4) )
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
} 
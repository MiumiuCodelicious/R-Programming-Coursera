corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  corr_vec <- numeric()
  
  library(stringr)
  
  complete_files <- complete(directory)
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  above_threshold <- subset(complete_files, nobs > threshold) 
  
  index <- 1
  for (i in above_threshold[,"id"]){
    file_name <- str_pad(i, 3, pad="0")
    file_path <- paste(directory[[1]], "/", file_name, ".csv", sep="")
    cur_file <- read.csv(file = file_path, head = T, sep = ",")
    cur_complete <- cur_file[complete.cases(cur_file),]
    
    corr_vec[index] <- cor( cur_complete[, "sulfate"], cur_complete[, "nitrate"] )
    index <- index + 1
  }
  
  return(corr_vec)
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}
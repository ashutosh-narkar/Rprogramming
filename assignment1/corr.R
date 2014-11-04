corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    source("complete.R")
    complete_cases <- complete(directory)
    ids <- c() # ids of all files where number of complete cases is above threshold
    
    # get those files where number of complete
    # cases is above threshold
    # data frame returned by complete.R has two columns
    # 1) id and 2) nobs
    for (i in seq_len(nrow(complete_cases))){
        if (complete_cases[i,2] > threshold){
            ids <-  c(ids, complete_cases[i,1])
        }
    }
    
    correlation <- c()
    # find the correlation between sulfate and nitrate
    for (i in ids){
        filename <- sprintf("%03d.csv", i)      # 001 format
        file <- file.path(directory, filename)
        data <- read.csv(file)
        
        val <- cor(data[['sulfate']], data[['nitrate']], use = 'complete.obs')
        correlation <- c(correlation, val)
    }
    
    return(correlation)
    
}
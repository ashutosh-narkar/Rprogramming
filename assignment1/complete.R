complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ids <- c()
    nobs <- c()
    for (i in id){
        filename <- sprintf("%03d.csv", i)      # 001 format
        file <- file.path(directory, filename)
        data <- read.csv(file)
        
        # check every row of the data frame
        # and ensure no column has a missing value
        nobs_count <- 0
        for (j in seq_len(nrow(data))){
            col_vals <- data[j,]                # get the entire jth row
            count <- 0
            for (val in col_vals){
                if (is.na(val)){
                    count <- count + 1
                }
            }
            # no missing values in row
            if(count == 0){
                nobs_count <- nobs_count + 1
            }
                        
        }
        ids <- c(ids, i)
        nobs <- c(nobs, nobs_count)
              
    } 
    data.frame(id=ids, nobs=nobs)
    
}
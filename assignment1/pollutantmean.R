pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    mean_vals <- c()
    for (i in id){
        filename <- sprintf("%03d.csv", i)      # 001 format
        file <- file.path(directory, filename)
        data <- read.csv(file)
        col <- (data[[pollutant]]) # extract the particular col
        col <- col[!is.na(col)]    # remove missing values
        mean_vals <- c(mean_vals, col)        
    } 
    mean(mean_vals)
    
}
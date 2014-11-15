rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with given rank 30-day death
    ## rate
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states <- data[,7] # column 7 lists the 2 letter State code in which the hospital is located
    outcomes = c('heart attack', 'heart failure', 'pneumonia')    # possible outcomes
    
    # check if a valid state is passed
    if (!is.element(state, states)){
        stop('invalid state')
    }
    
    # check if a valid outcome is passed
    if (!is.element(outcome, outcomes)){
        stop('invalid outcome')
    }
    
    
    if (outcome == 'heart attack'){
        hospital_name <- get_hospital_name(state, 11, data, num)   # col 11 lists Hospital 30-Day Death (Mortality) Rates from Heart Attack   
    }
    
    if (outcome == 'heart failure'){
        hospital_name <- get_hospital_name(state, 17, data, num)   # col 17 lists  Hospital 30-Day Death (Mortality) Rates from Heart Failure   
    }
    
    if (outcome == 'pneumonia'){
        hospital_name <- get_hospital_name(state, 23, data, num)   # col 23  lists Hospital 30-Day Death (Mortality) Rates from Pneumonia 
    }
    
    hospital_name
}


get_hospital_name <- function(state, outcome_col, data, rank){
    ## Given the state and outcome column
    ## return the hospital name with the given rank for the 30-day death rate
    
    # get all hospitals in a particular state alongwith the mortality data for outcome
    hospital_name <- c()
    mortality_rate <- c()
    for (i in seq_len(nrow(data))){
        if (state == data[i,7]){
            if(!is.na(data[i, outcome_col])){
                hospital_name <- c(hospital_name, data[i,2])       # cloumn 2 list hospital names
                mortality_rate <- c(mortality_rate, data[i, outcome_col])
                
            }
        }
    }
    mortality_rate <- as.numeric(mortality_rate) # Because we originally read the data in as character
                                                 # (by specifying colClasses = "character") we need to
                                                 # coerce the column to be numeric
    rate_hospital <- data.frame(rate=mortality_rate, name=hospital_name, stringsAsFactors=FALSE)
        
    # sort the  hospitals in ascending order of mortality rates, second col is provided
    # to break ties in case 2 elements in the first col are same
    rate_hospital <- rate_hospital[order(rate_hospital[,1], rate_hospital[,2]), ]
    
       
    if(rank == 'best'){
        ranked_hospital <- rate_hospital[1,2]
    }
    
    else if(rank == 'worst'){
        # sort the  hospitals in descending order of mortality rates
        rate_hospital <- rate_hospital[order(-rate_hospital[,1], rate_hospital[,2]), ]
        ranked_hospital <- rate_hospital[1,2]
    }
    
    else{
        ranked_hospital <- rate_hospital[rank,2]
    }
    
    ranked_hospital
    
}

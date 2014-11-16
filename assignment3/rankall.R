rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that outcome arg is valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states <- data[,7] # column 7 lists the 2 letter State code in which the hospital is located
    outcomes = c('heart attack', 'heart failure', 'pneumonia')    # possible outcomes
    
    # check if a valid outcome is passed
    if (!is.element(outcome, outcomes)){
        stop('invalid outcome')
    }
   
    state_list <- c()
    hospitals <- c()
    
    states <- unique(states)  # remove duplicate data
    for (state in states){
        # get all entries correponding to the particular state
        info <- subset(data, State==state)
        
        if (outcome == 'heart attack'){
            hospital_name <- get_hospital_name(11, info, num)   # col 11 lists Hospital 30-Day Death (Mortality) Rates from Heart Attack   
        }
        
        if (outcome == 'heart failure'){
            hospital_name <- get_hospital_name(17, info, num)   # col 17 lists  Hospital 30-Day Death (Mortality) Rates from Heart Failure   
        }
        
        if (outcome == 'pneumonia'){
            hospital_name <- get_hospital_name(23, info, num)   # col 23  lists Hospital 30-Day Death (Mortality) Rates from Pneumonia 
        }
        
        state_list <- c(state_list, state)
        hospitals <- c(hospitals, hospital_name)
        
    }
    
    data.frame(hospital=hospitals, state=state_list)
    
}

get_hospital_name <- function(outcome_col, data, rank){
    ## return the hospital name with the given rank for the 30-day death rate
    
    # get all hospitals in a particular state alongwith the mortality data for outcome
    # ignore hospitals for which data is not available
    hospital_name <- c()
    mortality_rate <- c()
    
    data <- subset(data, !is.na(as.numeric(data[ ,outcome_col])))
    mortality_rate <- as.numeric(data[ ,outcome_col])       # Because we originally read the data in as character
                                                            # (by specifying colClasses = "character") we need to
                                                            # coerce the column to be numeric
        
    hospital_name <- data[,2]                               # column 2 list hospital names
    
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
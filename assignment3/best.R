best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
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
   
    # state specific data
    data <- subset(data, State==state)
   
   if (outcome == 'heart attack'){
       hospital_name <- get_hospital_name(11, data)   # col 11 lists Hospital 30-Day Death (Mortality) Rates from Heart Attack   
   }
   
   if (outcome == 'heart failure'){
       hospital_name <- get_hospital_name(17, data)   # col 17 lists  Hospital 30-Day Death (Mortality) Rates from Heart Failure   
   }
   
   if (outcome == 'pneumonia'){
       hospital_name <- get_hospital_name(23, data)   # col 23  lists Hospital 30-Day Death (Mortality) Rates from Pneumonia 
   }
   hospital_name
}


get_hospital_name <- function(outcome_col, data){
    ## Given the state specific data and outcome column
    ## return the hospital name with lowest 30-day death rate
    
    # get all hospitals in a particular state alongwith the mortality data for outcome
    # ignore hospitals for which data is not available
    hospital_name <- c()
    mortality_rate <- c()
    
    data <- subset(data, !is.na(as.numeric(data[ ,outcome_col])))
    mortality_rate <- as.numeric(data[ ,outcome_col])       # Because we originally read the data in as character
                                                            # (by specifying colClasses = "character") we need to
                                                            # coerce the column to be numeric
    
    hospital_name <- data[,2]  
    
    
    index_min <- which.min(mortality_rate)
    hospital_name[index_min]
}

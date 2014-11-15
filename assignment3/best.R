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
   
   hospital_name <- ''
   if (outcome == 'heart attack'){
       hospital_name <- get_hospital_name(state, 11, data)   # col 11 lists Hospital 30-Day Death (Mortality) Rates from Heart Attack   
   }
   
   if (outcome == 'heart failure'){
       hospital_name <- get_hospital_name(state, 17, data)   # col 17 lists  Hospital 30-Day Death (Mortality) Rates from Heart Failure   
   }
   
   if (outcome == 'pneumonia'){
       hospital_name <- get_hospital_name(state, 23, data)   # col 23  lists Hospital 30-Day Death (Mortality) Rates from Pneumonia 
   }
   hospital_name
}


get_hospital_name <- function(state, outcome_col, data){
    ## Given the state and outcome column
    ## return the hospital name with lowest 30-day death rate
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
    index_min <- which.min(mortality_rate)
    hospital_name[index_min]
}

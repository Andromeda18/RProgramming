# outcome <- read.csv(file = "outcome-of-care-measures.csv", 
#                     colClasses = "character")
# head(outcome)
# outcome[, 11] <- as.numeric(outcome[, 11])
# hist(outcome[, 11])

best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv(file = "outcome-of-care-measures.csv", 
                        colClasses = "character")
    ## Check that state and outcome are valid
    states <- unique(outcome_data$State)
    if(!(state %in% states))
    {
        stop("invalid state")
    }
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% outcomes))
    {
        stop("invalid outcome")
    }
    
    ### Return hospital name in that state with lowest 30-day death
    ### rate
    
    ##Process outcome string in order to select the corresponding column
    
    #split the string by word
    strvec <- strsplit(outcome, " ")[[1]]
    
    #Change the first letter of each word to uppercase
    for(i in 1:length(strvec))
    {
        strvec[i] <- paste0(toupper(substr(strvec[i], 1, 1)),
               substr(strvec[i], 2, nchar(strvec[i])))
    }

    #Concatenate the words together, separated by a dot.
    outcome <- paste(strvec, collapse = ".")

    #Concatenate the processed outcome string with the column's prefix string
    col_name <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.", outcome)
    
    ##Return the hospital with the lowest death, given the state and outcome.
    
    #Convert the outcome column to numeric
    outcome_data[,col_name] <- as.numeric(outcome_data[,col_name])
    
    #Select the data by state
    death_rate_by_state <- outcome_data[outcome_data$State == 
                                                        state,]
    #Remove NAs
    death_rate_by_state <- death_rate_by_state[!is.na(death_rate_by_state[col_name]),]
    
    #Find the lowest death rate
    lowest_death_rate <- min(death_rate_by_state[,col_name])
    
    #Select the hospitals with the lowest death rate
    best_hospitals <- death_rate_by_state[death_rate_by_state[col_name]==lowest_death_rate,]
    
    #Sort the hospitals alphabetically and return the one at the top.
    best_hp <- sort(best_hospitals$Hospital.Name)[1]
    best_hp
}
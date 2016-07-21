rankhospital <- function(state, outcome, num = "best") {
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
    
    ### Return hospital name in that state with the given rank
    ### 30-day death rate
    
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
    
    
    ##Order hospitals by death rate and name (the name is used to break ties)
    
    #Convert the outcome column to numeric
    outcome_data[,col_name] <- as.numeric(outcome_data[,col_name])
    
    #Select the data by state
    death_rate_by_state <- outcome_data[outcome_data$State == 
                                            state,]
    #remove NAs
    death_rate_by_state_no_nas <- 
        death_rate_by_state[!is.na(death_rate_by_state[col_name]),]
    
    #sort data by death rate and name
    death_rate_by_state_sorted <- 
        death_rate_by_state_no_nas[order(death_rate_by_state_no_nas[col_name], death_rate_by_state_no_nas$Hospital.Name),]
    
    #select hospital by rank
    if(num =="best")
    {
        result <- death_rate_by_state_sorted[1, 2]
    }
    else if(num =="worst")
    {
        result <- death_rate_by_state_sorted[nrow(death_rate_by_state_sorted), 2]
    }
    else if(num%%1==0 & num>0 & num<=nrow(death_rate_by_state_sorted))
    {
        result <- death_rate_by_state_sorted[num, 2]
    }
    else if(num%%1==0 & num>nrow(death_rate_by_state_sorted))
    {
        result <- NA
    }
    
    result
}
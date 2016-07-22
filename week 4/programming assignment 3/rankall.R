rankall <- function(outcome, num = "best") {
    ### Read outcome data
    outcome_data <- read.csv(file = "outcome-of-care-measures.csv", 
                             colClasses = "character")
    
    ### Check that state, outcome and num are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% outcomes))
    {
        stop("invalid outcome")
    }
    
    if(is.numeric(num))
    {
        if(num%%1!=0 | num<0)
        {
            stop("invalid number")
        }
    }
    
    
    ### For each state, find the hospital of the given rank
    
    ## Process outcome string in order to select the corresponding column
    
    # split the string by word
    strvec <- strsplit(outcome, " ")[[1]]
    
    # Change the first letter of each word to uppercase
    for(i in 1:length(strvec))
    {
        strvec[i] <- paste0(toupper(substr(strvec[i], 1, 1)),
                            substr(strvec[i], 2, nchar(strvec[i])))
    }
    
    # Concatenate the words together, separated by a dot.
    outcome <- paste(strvec, collapse = ".")
    
    # Concatenate the processed outcome string with the column's prefix string
    col_name <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.", outcome)
    
    
    ## Group the data by state and order it by outcome
    
    # Convert the outcome column to numeric
    outcome_data[,col_name] <- as.numeric(outcome_data[,col_name])
    
    # Remove NAs
    outcome_data_no_nas <- outcome_data[!is.na(outcome_data[col_name]),]
    
    # Split data by state
    split_by_state <- split(outcome_data_no_nas, outcome_data_no_nas$State)
    
    # Apply a function to each group. This function sorts the data by outcome
    # and hospital name, and returns the hospital name corresponding to the 
    # specified rank.
    ordered_data <- lapply(split_by_state, function(state_df) {
        state_df_ordered <- state_df[
            order(state_df[col_name], state_df$Hospital.Name),]
        if(num == "best") num <- 1
        if(num == "worst") num <- nrow(state_df)
        state_df_ordered[num, 2]})
    
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    # Use the list returned by the previous function to create a data frame with
    # the specified structure.
    rankall_df <- data.frame(hospital=unlist(ordered_data), 
                             state=names(ordered_data))
    rankall_df
}
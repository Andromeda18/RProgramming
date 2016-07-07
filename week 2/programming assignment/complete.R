complete <- function(directory, id = 1:332)
{
  final_df <- data.frame(id=numeric(), nobs=numeric())
  
  for(elem in id)
  {
    filePath <- paste0(directory, '/', sprintf("%03d", elem), ".csv")
    file <- read.csv(filePath, header=TRUE, sep=",")
    
    nCompleteCases <- sum(complete.cases(file))
    
    df <- data.frame(id=elem, nobs=nCompleteCases)
    final_df <- rbind(final_df, df)
  }
  
  final_df
}
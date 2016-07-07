pollutantmean <- function(directory, pollutant, id = 1:332)
{
  sumlength <- 0
  my_sum <- 0
  
  for(elem in id)
  {
    filePath <- paste0(directory, '/', sprintf("%03d", elem), ".csv")
    file <- read.csv(filePath, header=TRUE, sep=",")
    
    my_sum <- my_sum + sum(file[[pollutant]], na.rm = TRUE)
    sumlength <- sumlength + length(file[[pollutant]][!is.na(file[[pollutant]])])
  }
  
  my_sum / sumlength
  
}
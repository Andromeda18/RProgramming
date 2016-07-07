source("complete.R")
corr <- function(directory, threshold = 0)
{

  compcases <- complete(directory)
  corrs <- numeric()
  #files <- list.files(directory) #list all files in the directory
  
  for(i in 1:length(compcases$id))
  {
    if(compcases[i, 'nobs']>threshold)
    {
      filePath <- paste0(directory, '/', sprintf("%03d", compcases[i, 'id']), ".csv")
      file <- read.csv(filePath, header=TRUE, sep=",")
      
      corrs <- c(corrs, cor(file[['sulfate']], file[['nitrate']], use="complete.obs"))
 
    }
  }
  corrs
}
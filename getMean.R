getMean <- function(directory, id = 1:332){
  
  # Useless warning messages come up 
  # because of "Inf" and "-Inf" values within vectors
  options(warn = -1) 
  
  # Creates a list of all the files within the directory
  fileList <- list.files(path = directory, pattern = ".csv", full.names = FALSE)
  
  # Initializes numeric vectors where the values for
  # "mean" will be stored
  MeanVectorNitrate <- numeric()
  MeanVectorSulfate <- numeric()
  originalNitrate <- numeric()
  originalSulfate <- numeric()
  
  # Loops through all CSV files within the specified directory
  for(i in id){
    data <- read.csv(fileList[i])
    
    # Finds "mean" values for Sulfate and Nitrate
    MeanNitrate <- mean(data[,3], na.rm = TRUE)
    MeanSulfate <- mean(data[,2], na.rm = TRUE)
    
    # Creates an original vector of every value even if
    # has infinite, finite, or NaN values
    originalNitrate <- c(originalNitrate, MeanNitrate)
    originalSulfate <- c(originalSulfate, MeanSulfate)

    # Checks is NaN a boolean value of TRUE for the
    # pollutant and removes the NaN from the vector
    if(is.nan(MeanNitrate) == TRUE | is.nan(MeanSulfate) == TRUE){
      MeanVectorNitrate <- MeanVectorNitrate[MeanVectorNitrate != "NaN"]
      MeanVectorSulfate <- MeanVectorSulfate[MeanVectorSulfate != "NaN"]
    }
    
    # Simply adds the value to the specified vector
    else{
      MeanVectorNitrate <- c(MeanVectorNitrate, MeanNitrate)
      MeanVectorSulfate <- c(MeanVectorSulfate, MeanSulfate)
    }
  }
  
  # Calculates the Average Mean for Sulfate and Nitrate and
  # locates the nearest file location for the average mean
  averageNitrate <- (sum(MeanVectorNitrate) / length(MeanVectorNitrate))
  fileAvgNitrate <- which.min(abs(originalNitrate - averageNitrate))
  averageSulfate <- (sum(MeanVectorSulfate) / length(MeanVectorSulfate))
  fileAvgSulfate <- which.min(abs(originalSulfate - averageSulfate))

  # Locates the minimum and maximum mean file locations
  fileMinNitrate <- which(originalNitrate == min(MeanVectorNitrate))
  fileMaxNitrate <- which(originalNitrate == max(MeanVectorNitrate))
  fileMinSulfate <- which(originalSulfate == min(MeanVectorSulfate))
  fileMaxSulfate <- which(originalSulfate == max(MeanVectorSulfate))
  
  # Print Statement with all calculated and located values
  cat(" Sulfate Minimum Mean:", min(MeanVectorSulfate), "File:", fileList[fileMinSulfate], "\n")
  cat(" Sulfate Maximum Mean:", max(MeanVectorSulfate), "File:", fileList[fileMaxSulfate], "\n")
  cat(" Sulfate Average Mean:", averageSulfate, "File:", fileList[fileAvgSulfate], "\n", "\n")
  cat(" Nitrate Minimum Mean:", min(MeanVectorNitrate), "File:", fileList[fileMinNitrate], "\n")
  cat(" Nitrate Maximum Mean:", max(MeanVectorNitrate), "File:", fileList[fileMaxNitrate], "\n")
  cat(" Nitrate Average Mean:", averageNitrate, "File:", fileList[fileAvgNitrate], "\n")
  
  # Visualizes a relationship between mean values of Sulfate and Nitrate
  # concentrations using the linear regression function lm
  plot(MeanVectorSulfate, MeanVectorNitrate)
  abline(lm(MeanVectorSulfate ~ MeanVectorNitrate))
}
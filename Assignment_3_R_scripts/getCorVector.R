# 
# Loukas Mironidis
# Big Data Capstone Assignment 3
# 
# Description:
#   This R script will find the correlations asked in questions 7-11.
#   This script utilizes vectors and basic R commands.  The function
#   header takes the directory with specdata of all 332 csv files and
#   gives the option whether to loop through specific sets of csv's in
#   order to find the correlations.

getCorVector <- function(directory, id = 1:332){
  
  # Useless warning messages come up 
  # because of "Inf" and "-Inf" values within vectors
  options(warn = -1)  
  
  # Creates a list of all the files within the directory
  fileList <- list.files(path = directory, pattern = ".csv", full.names = FALSE)
  
  # Initializes numeric vectors where the values for
  # "max", "min", and "mean" will be stored
  MaxVectorNitrate <- numeric()
  MaxVectorSulfate <- numeric()
  MinVectorNitrate <- numeric()
  MinVectorSulfate <- numeric()
  MeanVectorNitrate <- numeric()
  MeanVectorSulfate <- numeric()

  # Loops through all CSV files within the specified directory
  for(i in id){
    data <- read.csv(fileList[i])
    
    # Finds "max", "min", "mean" values for Sulfate and Nitrate
    MeanNitrate <- mean(data[,3], na.rm = TRUE)
    MeanSulfate <- mean(data[,2], na.rm = TRUE)
    MaxNitrate <- max(data[,3], na.rm = TRUE)
    MaxSulfate <- max(data[,2], na.rm = TRUE)
    MinNitrate <- min(data[,3], na.rm = TRUE)
    MinSulfate <- min(data[,2], na.rm = TRUE)

    # Checks is infinite or is NaN a boolean value of TRUE for the
    # pollutant and removes the "Inf", "-Inf" and "NaN from the vector
    if(is.infinite(MaxNitrate) == TRUE | is.infinite(MaxSulfate) == TRUE){
      MaxVectorNitrate <- MaxVectorNitrate[MaxVectorNitrate != "Inf"]
      MaxVectorSulfate <- MaxVectorSulfate[MaxVectorSulfate != "Inf"]
    }
    if(is.infinite(MinNitrate) == TRUE | is.infinite(MinSulfate) == TRUE){
      MinVectorNitrate <- MinVectorNitrate[MinVectorNitrate != "Inf"]
      MinVectorSulfate <- MinVectorSulfate[MinVectorSulfate != "Inf"]
    }
    if(is.nan(MeanNitrate) == TRUE | is.nan(MeanSulfate) == TRUE){
      MeanVectorNitrate <- MeanVectorNitrate[MeanVectorNitrate != "NaN"]
      MeanVectorSulfate <- MeanVectorSulfate[MeanVectorSulfate != "NaN"]
    }
    
    # Simply adds the value to the specified vector
    else{
      MaxVectorNitrate <- c(MaxVectorNitrate, MaxNitrate)
      MaxVectorSulfate <- c(MaxVectorSulfate, MaxSulfate)
      MinVectorNitrate <- c(MinVectorNitrate, MinNitrate)
      MinVectorSulfate <- c(MinVectorSulfate, MinSulfate)
      MeanVectorNitrate <- c(MeanVectorNitrate, MeanNitrate)
      MeanVectorSulfate <- c(MeanVectorSulfate, MeanSulfate)
    }
  }
  
    # Number 7: Find a correlation coefficient of Max value of Sulfate and Max value of Nitrate
    # concentrations over all locations.
    corMaxSulfateNitrate <- cor(MaxVectorSulfate, MaxVectorNitrate, use = 'complete.obs')

    # Number 8: Find a correlation coefficient of Min value of Sulfate and Min value of Nitrate
    # concentrations over all locations.
    corMinSulfateNitrate <- cor(MinVectorSulfate, MinVectorNitrate, use = 'complete.obs')

    # Number 9: Find a a correlation coefficient of Mean value of Sulfate and Mean value of Nitrate
    # concentrations over all locations
    corMeanSulfateNitrate <- cor(MeanVectorSulfate, MeanVectorNitrate, use = 'complete.obs')

    # Number 10: Calculate a correlation coefficient between vectors from number 7 and 8
    corMaxMinSulfate <- cor(MaxVectorSulfate, MinVectorSulfate, use = 'complete.obs')
    corMaxMinNitrate <- cor(MaxVectorNitrate, MinVectorNitrate, use = 'complete.obs')
    corMaxNitrateMinSulfate <- cor(MaxVectorNitrate, MinVectorSulfate, use = 'complete.obs')
    corMaxSulfateMinNitrate <- cor(MaxVectorSulfate, MinVectorNitrate, use = 'complete.obs')

    # Number 11: Calculate a correlation coefficient between vectors from number 9 and 10
    corMeanSulfateMaxSulfate <- cor(MeanVectorSulfate, MaxVectorSulfate, use = 'complete.obs')
    corMeanSulfateMaxNitrate <- cor(MeanVectorSulfate, MaxVectorNitrate, use = 'complete.obs')
    corMeanSulfateMinSulfate <- cor(MeanVectorSulfate, MinVectorSulfate, use = 'complete.obs')
    corMeanSulfateMinNitrate <- cor(MeanVectorSulfate, MinVectorNitrate, use = 'complete.obs')
    corMeanNitrateMaxSulfate <- cor(MeanVectorNitrate, MaxVectorSulfate, use = 'complete.obs')
    corMeanNitrateMaxNitrate <- cor(MeanVectorNitrate, MaxVectorNitrate, use = 'complete.obs')
    corMeanNitrateMinSulfate <- cor(MeanVectorNitrate, MinVectorSulfate, use = 'complete.obs')
    corMeanNitrateMinNitrate <- cor(MeanVectorNitrate, MinVectorNitrate, use = 'complete.obs')

    # Print Statement for all calculated values
    cat(' Correlation Max:', corMaxSulfateNitrate, '\n', 'Correlation Min:', corMinSulfateNitrate, '\n', 'Correlation Mean:', corMeanSulfateNitrate, '\n', '\n')
    cat(' Correlation between vectors from Number 7 and 8:', '\n', corMaxMinSulfate, corMaxMinNitrate, corMaxNitrateMinSulfate, corMaxSulfateMinNitrate, '\n', '\n')
    cat(' Correlation between vectors from Number 9 and 10:', '\n', corMeanSulfateMaxSulfate, corMeanSulfateMaxNitrate, corMeanSulfateMinSulfate, corMeanSulfateMinNitrate,'\n')
    cat('', corMeanNitrateMaxSulfate, corMeanNitrateMaxNitrate, corMeanNitrateMinSulfate, corMeanNitrateMinNitrate)
}
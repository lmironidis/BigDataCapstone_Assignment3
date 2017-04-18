# 
# Loukas Mironidis
# Big Data Capstone Assignment 3
# 
# Description:
#   This R script will find the simple summary statistics answering
#   number 1 on the assignment.  The function header takes the directory 
#   with specdata of all 332 csv files and gives the option whether 
#   to loop through specific sets of csv's in order to find the simple
#   summary statistics of the csv files in specdata.  This script
#   will also create a text file with all summary statistics with location
#   to the specified directory.

getSummary <- function(directory, id = 1:332){
  
  # Creates a list of all the files within the directory
  fileList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  # Creates a text file that will store summary statisitics
  logfile = '/home/loukas/Desktop/summary_data.txt'
  
  # Loops through all CSV files within the specified directory
  for(i in id){
    data <- read.csv(fileList[i])
    
    # Will print out location and summary statistics
    # for all locations
    print(fileList[i])
    print(summary(data))
    
    # Writes summary statistics to text file
    cat(fileList[i], file = logfile, append = TRUE, sep = '\n')
    write.table(summary(data), file = logfile, append = TRUE, sep = '\t')
  }
}
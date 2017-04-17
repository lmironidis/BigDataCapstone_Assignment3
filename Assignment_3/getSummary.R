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
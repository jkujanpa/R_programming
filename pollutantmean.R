pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    # Init values vector
    values <- NULL
  
    # Go through selected mnitor ids
    for(i in id) {
        
        # Generaate filename for file
        monitor <- formatC(i, width = 3, flag = "0")
        filename <- paste(monitor, "csv", sep = ".")
        filename <- paste(directory, filename, sep = "/")
        # Read the file 
        data <- read.csv(filename)

        # Store the measurement values of given pollutant
        if (pollutant == "sulfate") {
            data <- data[,2]
        } else { #nitrate
            data <- data[,3]
        }
        
        # Skip NA values
        data <- data[complete.cases(data)]
        # Add new values to vector
        values <- c(values, data)
    }
  
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    # Calculate the mean of all values
    mean(values)  
}
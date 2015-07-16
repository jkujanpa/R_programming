pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

    
    values <- NULL
  
    for(i in id) {
        
        print(i)
        
        monitor <- formatC(i, width = 3, flag = "0")
        filename <- paste(monitor, "csv", sep = ".")
        filename <- paste(directory, filename, sep = "/")
        
        print(filename)

        data <- read.csv(filename)
        
        if (pollutant == "sulfate") {
            data <- data[,2]
        } else { #nitrate
            data <- data[,3]
        }
        
        data <- data[complete.cases(data)]
        values <- c(values, data)
    }
  
  
    
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  
    mean(values)  
  
}
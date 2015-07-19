complete <- function(directory, id = 1:332) {
  
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    # init data vectors
    ids <- NULL
    nobs <- NULL
    
    # Go through selected mnitor ids
    for(i in id) {
        
        # Generaate filename for file
        monitor <- formatC(i, width = 3, flag = "0")
        filename <- paste(monitor, "csv", sep = ".")
        filename <- paste(directory, filename, sep = "/")
        # Read the file 
        data <- read.csv(filename)
        # Skip NA values
        data <- data[complete.cases(data),]
        
        # Update data for each monitor
        ids <- c(ids, i)
        nobs <- c(nobs, nrow(data))
    }

    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # Put all to one dataframe    
    data.frame(id = ids, nobs = nobs)
}
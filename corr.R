corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    # Init correlation vector
    correlations <- NULL
    
    # Go through all monitor ids
    for(i in 1:332) {
        
        # Generaate filename for file
        monitor <- formatC(i, width = 3, flag = "0")
        filename <- paste(monitor, "csv", sep = ".")
        filename <- paste(directory, filename, sep = "/")
        # Read the file 
        data <- read.csv(filename)
        # Skip NA values
        data <- data[complete.cases(data),]

        # Check if there is enough valid observations
        if (nrow(data) > threshold) {
            # Calculate the correlation
            correlation <- cor(data[,2], data[,3], use ="pairwise.complete.obs")
            # Store to vector
            correlations <- c(correlations, correlation)
        }
    
    }
    
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    correlations
}
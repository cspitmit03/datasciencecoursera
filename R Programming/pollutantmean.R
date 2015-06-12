pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        files_list <- list.files(directory, full.names=TRUE) #create a list of files
        dat <- data.frame()
        for (i in 1:length(files_list)) {
                dat <- rbind(dat, read.csv(files_list[i]))
        }

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        data_subset <- dat[, c(pollutant, "ID")]
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        data_sub2 <- subset(data_subset, ID %in% id)
        data_use <- data_sub2[ ,c(pollutant)]
        
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        mean(data_use, na.rm = TRUE)
        
        
}
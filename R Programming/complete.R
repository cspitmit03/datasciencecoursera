complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        files_list <- list.files(directory, full.names=TRUE) #create a list of files
        dat <- data.frame()
        for (i in 1:length(files_list)) {
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        data_omitted <- subset(na.omit(dat))
        data_use <- subset(data_omitted, ID %in% id)
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        nobs <- vector ()
        j <- length(id)
        count <- 0
        
        for (i in 1:j) {
                data_sub <- subset(data_use, data_use$ID == id[i])
                nobs[i] <- nrow(data_sub)
        }
        
        count <- data.frame(id, nobs)
        print(count)
}
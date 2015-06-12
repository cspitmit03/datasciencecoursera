corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        files_list <- list.files(directory, full.names=TRUE) #create a list of files
        dat <- data.frame()
        for (i in 1:length(files_list)) {
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        cases <- complete.cases(dat)
        datmod <- data.frame(dat, cases)
        full <- subset(datmod, cases == TRUE)
        monitorcount <- data.frame(table(full$ID))
        monitors_use <- subset(monitorcount, Freq>threshold)
        monitors <- monitors_use[,1]
        
        data_use <- subset(full, ID %in% monitors)
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        correlation <- vector ()
        j <- length(monitors)
        count <- 0
        
        for (i in 1:j) {
                data_sub <- subset(data_use, data_use$ID == monitors[i])
                correlation[i] <- cor(data_sub$sulfate, data_sub$nitrate)
        }
        
        count <- correlation
        
        if(length(count) < 2) count <- vector(mode = "numeric", length = 0)
        
        print(count)
        
}
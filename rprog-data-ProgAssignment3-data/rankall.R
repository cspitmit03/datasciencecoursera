## Cesar Espitia
## This script allows a user to determine what the best hospital is

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        types <- c("heart attack", "heart failure", "pneumonia")
        
        a1 <- data[,c(1,2)]
        a11 <- as.factor(data[,c(7)])
        a2 <- suppressWarnings(data.frame(lapply(data[,c(11,17,23)], as.numeric)))
        a3 <- data.frame(a1,a11,a2)
        colnames(a3) <- c("ID", "hospital", "state", "HAM", "HFM", "PM")
        statetore <- unique(a3$state)
        
        ## Check that state and outcome are valid

        if(!(outcome %in% types)) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        a4 <- subset(a3)
        if(outcome == "heart attack") {
                b <- a4[order(a4$state, a4$HAM, a4$hospital),]
                b0 <- test <- b[!is.na(b$HAM),]
                b1 <- b0[,c(2,3,4)]
                c <- split(b1, b1$state)
        }
        if(outcome == "heart failure") {
                b <- a4[order(a4$state, a4$HFM, a4$hospital, a4$state),]
                b0 <- test <- b[!is.na(b$HFM),]
                b1 <- b0[,c(2,3,5)]
                c <- split(b1, b1$state)
        }
        if(outcome == "pneumonia") {
                b <- a4[order(a4$state, a4$PM, a4$hospital),]
                b0 <- test <- b[!is.na(b$PM),]
                b1 <- b0[,c(2,3,6)]
                c <- split(b1, b1$state)
        }
        
        if(num == "best"){number <- 1}
        else if(num == "worst"){number <- 1}
        else {number <- as.numeric(num)}
        
        my.order <- function(l) {
                order(l[,3], l[,1])
        }
        
        my.orderworst <- function(l) {
                order(rev(l[,3]), rev(l[,1]))
        }
        
        if(num == "worst"){z <- lapply(c, my.orderworst)}
        else {z <- lapply(c, my.order)}
        z1 <- data.frame(matrix(unlist(z)))
        colnames(z1) <- c("rankings")
        
        h <- data.frame(b1,z1)
        
        h1 <- subset(h, rankings == number)
        
        newstate <- lapply(statetore, as.character)
        newstate1 <- data.frame(matrix(unlist(newstate)))
        colnames(newstate1) <- c("state")
        
        h2 <- merge(newstate1, h1, by.x = "state", by.y = "state", all = TRUE)
        h2[,c(2,1)]
}
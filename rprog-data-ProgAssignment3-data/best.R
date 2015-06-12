## Cesar Espitia
## This script allows a user to determine what the best hospital is

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        types <- c("heart attack", "heart failure", "pneumonia")
        states <- unique(data$State)
        a1 <- data[,c(1,2,7)]
        a2 <- suppressWarnings(data.frame(lapply(data[,c(11,17,23)], as.numeric)))
        a3 <- data.frame(a1,a2)
        colnames(a3) <- c("ID", "HN", "states", "HAM", "HFM", "PM")
        
        ## Check that state and outcome are valid
        if(!(state %in% states)) {
                stop("invalid state")
        }
        if(!(outcome %in% types)) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        a4 <- subset(a3, states == state)
        if(outcome == "heart attack") {
                b <- na.omit(a4[order(a4$HAM, a4$HN),])
        }
        if(outcome == "heart failure") {
                b <- na.omit(a4[order(a4$HFM, a4$HN),])
        }
        if(outcome == "pneumonia") {
                b <- na.omit(a4[order(a4$PM, a4$HN),])
        }
        return(b[1,2])
        
}
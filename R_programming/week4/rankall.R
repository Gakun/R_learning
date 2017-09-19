rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (outcome == 'heart attack') {
        data <- data[, c(2, 7, 11)]
    }
    else if (outcome == 'heart failure') {
        data <- data[, c(2, 7, 17)]
    }
    else if (outcome == 'pneumonia') {
        data <- data[, c(2, 7, 23)]
    }
    else {
        stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    ## If data[,3] are not coerce to character when read csv file, it would be a factor.
    ## A factor "not available would be changed to number by as.numeric()
    data[, 3] <- as.numeric(data[, 3])
    ## Split dataframe into groups by State 
    dataByState <- split(data, data$State)
    ## Order each groups by their outcome and name
    dataByState <- lapply(dataByState, function(x) x[order(x[, 3], x[, 1], na.last = NA),])
    ## Add rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    oneByState <- lapply(dataByState, function(x) {
        if (num == "best") x[1, 1:2]
        else if (num == "worst") x[nrow(x), 1:2]
        else x[num, 1:2]
        })
    result <- do.call("rbind", oneByState)
    colnames(result) <- c("hospital", "state")
    result
}


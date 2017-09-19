best <- function(state, outcome) {
    ## Read Outcome Data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!is.element(state, data$State)) {
        stop("invalid state")
    }
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death rate
    if (outcome == 'heart attack') {
        data <- data[data$State == state, c(2, 11)]
    }
    if (outcome == 'heart failure') {
        data <- data[data$State == state, c(2, 17)]
    }
    if (outcome == 'pneumonia') {
        data <- data[data$State == state, c(2, 23)]
    }
    data[, 2] <- as.numeric(data[, 2])
    data[order(data[, 2], data[, 1])[1], 1]
}
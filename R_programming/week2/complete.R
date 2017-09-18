complete <- function(directory, id = 1:332) {
    result <- data.frame()
    for (i in id) {
        data <- read.csv(paste(directory, '/', formatC(i, width = 3, flag = "0"), ".csv", sep = ""))
        good <- complete.cases(data)
        result <- rbind(result, c(i, nrow(data[good,])))
    }
    colnames(result) <- c("id", "nobs")
    result
}
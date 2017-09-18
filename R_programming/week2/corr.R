corr <- function(directory, threshold = 0) {
    correlation <- numeric()
    comp <- complete(directory)
    ids <- comp[comp$nobs > threshold,][,"id"]
    for (i in ids) {
        data <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = ""))
        data <- data[complete.cases(data),]
        correlation <- c(correlation, cor(data[,'sulfate'], data[,'nitrate']))
    }
    correlation
}
add2 <- function(x, y) {
        x + y
}

abofve10 <- function(x) {
        use <- x > 10
        x[use]
}

columnmean <- function(x, removeNA = TRUE) {
        nc <- ncol(x)
        means <- numeric()
        for(i in 1:nc) {
                means[i] <- mean(x[,i], na.rm = removeNA)
        }
        means
}
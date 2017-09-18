pollutantmean <- function(directory, pollutant, id = 1:332) {
    final.dataset <- data.frame()
    for (i in id) {
        dataset <- read.csv(paste(directory, "/", formatC(i, width=3, flag="0"), '.csv', sep = ""))
        final.dataset <- rbind(final.dataset, dataset)
    }
    final.data <- final.dataset[[pollutant]]
    mean(final.data[!is.na(final.data)])
}

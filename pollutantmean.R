pollutantmean <- function(directory, pollutant, id = 1:332) {
        files_full <- list.files(directory, full.names = TRUE) ## creates list of paths
        dat <- data.frame() ## creates a data frame
        for (i in id) {
                dat <- rbind(dat, read.csv(files_full[i])) ## binds files in directory into one data frame
        }
        mean(dat[, pollutant], na.rm = TRUE) ## calculates mean for desired pollutant
}
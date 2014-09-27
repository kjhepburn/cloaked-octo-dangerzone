corr <- function(directory, threshold = 0) { ##begin function
        result <- numeric(0)
        files_full <- list.files(directory, full.names = TRUE) ##creates a list of all files in the directory
        for(i in 1:length(files_full)) {
                dat <- read.csv(files_full[i])
                dat <- na.omit(dat)
                log_cases <- complete.cases(dat)
                num_cases <- sum(log_cases)
                if (num_cases > threshold) {
                        output1 <- cor(dat[, "sulfate"], dat[, "nitrate"])
                        result <- append(result, output1)
                }
        }
        result
}
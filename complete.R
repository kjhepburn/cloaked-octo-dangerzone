complete <- function(directory, id = 1:332) {
        files_full <- list.files(directory, full.names = TRUE) ##creates a data frame of all files in the directory
        dat <- data.frame() ##creates an overall data frame
        for (i in id) {
                dat <- rbind(dat, read.csv(files_full[i])) ##combines data into single data frame
        }
        full_cases <- numeric(1)
        final_dat <- data.frame()
        for (j in id) {
                id_dat <-  read.csv(files_full[j]) 
                log_cases <- complete.cases(id_dat)
                full_cases <- sum(log_cases)
                temp_dat <- data.frame(id = j, nobs = full_cases)
                final_dat <- rbind(final_dat, temp_dat)
        }
        final_dat
}
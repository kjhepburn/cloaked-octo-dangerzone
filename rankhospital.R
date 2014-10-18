rankhospital <- function(state, outcome, num) {
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ##reads data into vector
        state_list <- state.abb ##validity testing
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        match_abb <- state %in% state_list
        match_outcome <- outcome %in% outcome_list
        if(match_abb == FALSE) {
                stop("invalid state")
        }
        if(match_outcome == FALSE) {
                stop("invalid outcome")
        }
        if(outcome == "heart attack") {
                subset_dat <- data.frame(dat$Hospital.Name, suppressWarnings(as.numeric(dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])), dat$State, stringsAsFactors = FALSE)
                colnames(subset_dat) <- c("name", "rate", "state")
                subset_dat <- subset_dat[which(subset_dat$state == state), ]
                final_dat <- subset_dat[order(subset_dat$rate, subset_dat$name, na.last = NA), ]
                if(num == "best") {return(final_dat[1, "name"])}
                if(num == "worst") {return(final_dat[nrow(final_dat), "name"])}
                if(num > nrow(final_dat)) {return(NA)}
                return(final_dat[num, "name"])
        }
        if(outcome == "heart failure") {
                subset_dat <- data.frame(dat$Hospital.Name, suppressWarnings(as.numeric(dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])), dat$State, stringsAsFactors = FALSE)
                colnames(subset_dat) <- c("name", "rate", "state")
                subset_dat <- subset_dat[which(subset_dat$state == state), ]
                final_dat <- subset_dat[order(subset_dat$rate, subset_dat$name), ]
                if(num == "best") {return(final_dat[1, "name"])}
                if(num == "worst") {return(final_dat[nrow(final_dat), "name"])}
                if(num > nrow(final_dat)) {return(NA)}
                return(final_dat[num, "name"])
        }
        if(outcome == "pneumonia") {
                subset_dat <- data.frame(dat$Hospital.Name, suppressWarnings(as.numeric(dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])), dat$State, stringsAsFactors = FALSE)
                colnames(subset_dat) <- c("name", "rate", "state")
                subset_dat <- subset_dat[which(subset_dat$state == state), ]
                final_dat <- subset_dat[order(subset_dat$rate, subset_dat$name), ]
                if(num == "best") {return(final_dat[1, "name"])}
                if(num == "worst") {return(final_dat[nrow(final_dat), "name"])}
                if(num > nrow(final_dat)) {return(NA)}
                return(final_dat[num, "name"])
        }
}
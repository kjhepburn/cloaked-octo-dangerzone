best <- function(state, outcome) {
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
                suppressWarnings(as.numeric(dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])) 
                subset_dat <- dat[which(dat$State == state), ]
                lowest_ha <- suppressWarnings(which.min(subset_dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]))
                return(subset_dat[lowest_ha, "Hospital.Name"])
        }
        if(outcome == "heart failure") {
                suppressWarnings(as.numeric(dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])) 
                subset_dat <- dat[which(dat$State == state), ]
                lowest_hf <- suppressWarnings(which.min(subset_dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]))
                return(subset_dat[lowest_hf, "Hospital.Name"])
        }
        if(outcome == "pneumonia") {
                suppressWarnings(as.numeric(dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])) 
                subset_dat <- dat[which(dat$State == state), ]
                lowest_pn <- suppressWarnings(which.min(subset_dat[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]))
                return(subset_dat[lowest_pn, "Hospital.Name"])
        }
}
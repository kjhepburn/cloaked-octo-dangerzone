rankall <- function(outcome, num = "best") {
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available") ##reads data into vector
        state_list <- c(state.abb, "DC", "VI") ##validity testing
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        match_outcome <- outcome %in% outcome_list
        if(match_outcome == FALSE) {stop("invalid outcome")}
        pen_dat <- data.frame(dat$Hospital.Name, dat$State, dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, stringsAsFactors = FALSE)
        colnames(pen_dat) <- c("hospital", "state", "heart.attack", "heart.failure", "pneumonia")
        if(outcome == "heart attack") {
                ult_dat <- data.frame(hospital = as.character(), state = as.character(), stringsAsFactors = FALSE)
                for(i in state_list) {
                        state_dat <- pen_dat[which(pen_dat$state == i),]
                        state_dat <- state_dat[order(as.numeric(state_dat$heart.attack), state_dat$hospital, na.last = NA), ]
                        if(num == "best") {
                                choose_dat <- data.frame(state_dat[1, "hospital"], i, stringsAsFactors = FALSE)
                        }
                        else if (num == "worst") {
                                choose_dat <- data.frame(state_dat[nrow(state_dat), "hospital"], i, stringsAsFactors = FALSE)
                        }
                        else choose_dat <- data.frame(state_dat[num, "hospital"], i, stringsAsFactors = FALSE)
                        ult_dat <- rbind.data.frame(ult_dat, choose_dat)
                }
                colnames(ult_dat) <- c("hospital", "state")
                rownames(ult_dat) <- ult_dat[, "state"]
                ult_dat <- ult_dat[order(ult_dat$state),]
                return(ult_dat)
        }
        if(outcome == "heart failure") {
                ult_dat <- data.frame(hospital = as.character(), state = as.character(), stringsAsFactors = FALSE)
                for(i in state_list) {
                        state_dat <- pen_dat[which(pen_dat$state == i),]
                        state_dat <- state_dat[order(as.numeric(state_dat$heart.failure), state_dat$hospital, na.last = NA), ]
                        if(num == "best") {
                                choose_dat <- data.frame(state_dat[1, "hospital"], i, stringsAsFactors = FALSE)
                        }
                        else if (num == "worst") {
                                choose_dat <- data.frame(state_dat[nrow(state_dat), "hospital"], i, stringsAsFactors = FALSE)
                        }
                        else choose_dat <- data.frame(state_dat[num, "hospital"], i, stringsAsFactors = FALSE)
                        ult_dat <- rbind.data.frame(ult_dat, choose_dat)
                }
                colnames(ult_dat) <- c("hospital", "state")
                rownames(ult_dat) <- ult_dat[, "state"]
                ult_dat <- ult_dat[order(ult_dat$state),]
                return(ult_dat)
        }
        if(outcome == "pneumonia") {
                ult_dat <- data.frame(hospital = as.character(), state = as.character(), stringsAsFactors = FALSE)
                for(i in state_list) {
                        state_dat <- pen_dat[which(pen_dat$state == i),]
                        state_dat <- state_dat[order(as.numeric(state_dat$pneumonia), state_dat$hospital, na.last = NA), ]
                        if(num == "best") {
                                choose_dat <- data.frame(state_dat[1, "hospital"], i, stringsAsFactors = FALSE)
                        }
                        else if (num == "worst") {
                                choose_dat <- data.frame(state_dat[nrow(state_dat), "hospital"], i, stringsAsFactors = FALSE)
                        }
                        else choose_dat <- data.frame(state_dat[num, "hospital"], i, stringsAsFactors = FALSE)
                        ult_dat <- rbind.data.frame(ult_dat, choose_dat)
                }
                colnames(ult_dat) <- c("hospital", "state")
                rownames(ult_dat) <- ult_dat[, "state"]
                ult_dat <- ult_dat[order(ult_dat$state),]
                return(ult_dat)
        }
}
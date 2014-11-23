## Read in the data
library(dplyr)
y_train <- read.table("train/y_train.txt")
x_train <- read.table("train/x_train.txt")
subject_train <- read.table("train/subject_train.txt")
y_test <- read.table("test/y_test.txt")
x_test <- read.table("test/x_test.txt")
subject_test <- read.table("test/subject_test.txt")

## Recombine the data and insert variable name file
dat <- rbind(x_train, x_test)
activity_names <- rbind(y_train, y_test)
subjects <- rbind(subject_train, subject_test)
features <- readLines("features.txt")
colnames(dat) <- features
dat1 <- dat[, grep("mean\\(\\)", colnames(dat))]
dat2 <- dat[, grep("std\\(\\)", colnames(dat))]
dat <- cbind(dat1, dat2, activity_names, subjects)
names(dat)[68] <- "Subject"
names(dat)[67] <- "Activity"

## Remove extraneous objects from memory
rm(activity_names, dat1, dat2, x_test, x_train, y_test, y_train, features, subject_test, subject_train, subjects)

## Transform numeric activities into actual activity names
dat$Activity <- sub("1", "walking", dat$Activity)
dat$Activity <- sub("2", "walking upstairs", dat$Activity)
dat$Activity <- sub("3", "walking downstairs", dat$Activity)
dat$Activity <- sub("4", "sitting", dat$Activity)
dat$Activity <- sub("5", "standing", dat$Activity)
dat$Activity <- sub("6", "laying", dat$Activity)

## Perform analysis: find mean of each variable for each activity and subject
group_by(dat, Activity, Subject)
dat2 <- summarise_each(dat1, funs(mean), -Activity, -Subject)

## Rename variables' abbreviations to actual words for ease of use
colnames(dat2) <- gsub("Acc", "Accelerometer", colnames(dat2))
colnames(dat2) <- gsub("mean\\(\\)", "mean", colnames(dat2))
colnames(dat2) <- gsub("std\\(\\)", "StandardDeviation", colnames(dat2))
colnames(dat2) <- gsub("Gyro", "Gyroscope", colnames(dat2))
colnames(dat2) <- gsub("BodyBody", "Body", colnames(dat2))
colnames(dat2) <- gsub("tBody", "timeBody", colnames(dat2))
colnames(dat2) <- gsub("Mag", "Magnitude", colnames(dat2))
colnames(dat2) <- gsub("fBody", "FastFourierBody", colnames(dat2))
colnames(dat2) <- gsub("tGravity", "timeGravity", colnames(dat2))

## Create data file for export
write.table(dat2, "c:/cloaked-octo-dangerzone/SummarizedPhoneData.txt", row.name = FALSE)

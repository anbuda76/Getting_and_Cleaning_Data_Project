################################################################################################################################################
# Start Peer-graded Assignment: Getting and Cleaning Data Course Project

# Clean up workspace
rm(list = ls())

# load library
library(plyr) # load plyr first 
library(data.table) # a prockage that handles dataframe better
library(dplyr) # for fancy data table manipulations and organization

################################################################################################################################################
# Download the file and put the file in the temp folder 

temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
unzip(temp, list = TRUE)

################################################################################################################################################
# Read in the data from files

features        <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))            #List of all features

x_train         <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))       #Training set
y_train         <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))       #Training labels
subjectTrain    <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt")) #subject_train

x_test          <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))         #Test set
y_test          <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))         #Test labels
subjectTest     <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))   #subject_test

################################################################################################################################################
# Fix column names

colnames(x_train) <- t(features[2])
colnames(x_test) <- t(features[2])

colnames(y_train) <- c("activityId")
colnames(y_test) <- c("activityId")

colnames(subjectTrain) <- c("subject")
colnames(subjectTest) <- c("subject")

################################################################################################################################################
# Assignment 1
# Merge the training and test sets to create one data set

xData  <- rbind(x_train, x_test)

yData  <- rbind(y_train,y_test)
subject_data <- rbind(subjectTrain, subjectTest)


################################################################################################################################################
# Assignment 2
# Extracts only the measurements on the mean and standard deviation for each measurement.

# select only mean and std measurements
select <- grep("mean\\(\\)|std\\(\\)", features[, 2])

# apply the selection to the dataset
finalData <- xData[, select]

finalData <- cbind(yData, subject_data, finalData)

################################################################################################################################################
# Assignment 3
# Uses descriptive activity names to name the activities in the data set

# Read in the data from file
activity_labels <- read.table("./activity_labels.txt", header = FALSE)  # activity name.

# Fix column names
colnames(activity_labels) <- c("activityId", "activityDesc")

# Merge the data set
finalData <- merge(finalData, activity_labels, by = "activityId")

################################################################################################################################################
# Assignment 4
# Appropriately labels the data set with descriptive variable names.

# Cleaning up the variable names
names(finalData) <- gsub("^t", "time", names(finalData))
names(finalData) <- gsub("^f", "frequency", names(finalData))
names(finalData) <- gsub("Acc", "Accelerometer", names(finalData))
names(finalData) <- gsub("Gyro", "Gyroscope", names(finalData))
names(finalData) <- gsub("Mag", "Magnitude", names(finalData))
names(finalData) <- gsub("BodyBody", "Body", names(finalData))

# check the names
names(finalData)

################################################################################################################################################
# Assignment 5
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata <- aggregate(. ~subject + activityDesc, finalData, mean)
tidydata <-tidydata[order(tidydata$subject,tidydata$activityDesc),]

# export the tidydata set
write.table(tidydata, file = "C:/Getting and Cleaning Data/UCI HAR Dataset/tidydata.txt",row.name=FALSE)
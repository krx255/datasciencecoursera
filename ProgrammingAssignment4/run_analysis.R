# install packages
install.packages("knitr")
install.packages("rmarkdown")
library(knitr)


library(utils)
require(data.table)
library(dplyr)


setwd("C:\\Coursera\\R\\GettingAndCleaningData\\UCI HAR Dataset")

# run only once
#url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(url=url, destfile="dataset.zip", method="curl")
#unzip(zipfile = "dataset.zip")


# Gather Activity files
ytrain <- read.table(".\\train\\y_train.txt")
ytest <- read.table(".\\test\\y_test.txt")
activity <- rbind(ytrain, ytest)
names(activity) <- c("activity")


# gather Subject files
subjecttrain <- read.table(".\\train\\subject_train.txt")
subjecttest <- read.table(".\\test\\subject_test.txt")
subject <- rbind(subjecttrain, subjecttest)
names(subject) <- c("subject")

# Gather Features files
xtrain <- read.table(".\\train\\X_train.txt")
xtest <- read.table(".\\test\\X_test.txt")
features <- rbind(xtrain, xtest)
FeaturesNames <- read.table(file = "features.txt")
names(features) <- FeaturesNames$V2


# 1. Merges the training and the test sets to create one data set.
data <- cbind(subject, activity, features) 


# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
subsetFeaturesNames<-FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)]

# 3. Uses descriptive activity names to name the activities in the data set 

selectedNames <- c(as.character(subsetFeaturesNames), "subject", "activity" )
data <- subset(data,select=selectedNames)

# replace activity labels 1 ..6 with their respective names
ActivityLabels <- read.table(".\\activity_labels.txt")
for (i in 1:length(ActivityLabels$V1)){
  data$activity <- gsub(ActivityLabels[i,1], ActivityLabels[i,2], as.factor(data$activity))
}

# check replacement
#data$activity


# 4. Appropriately labels the data set with descriptive variable names.

# From freatures info file : (prefix 't' to denote time)
names(data)<-gsub("^t", "time", names(data))
# From freatures info file : (prefix 'f' to denote frequency domain signals)
names(data)<-gsub("^f", "frequency", names(data))
# Acc should be replaced with Accelerometer
names(data)<-gsub("Acc", "Accelerometer", names(data))
# Gyro should be replaced with Gyroscope
names(data)<-gsub("Gyro", "Gyroscope", names(data))
# Mag should be replaced with Magnitude
names(data)<-gsub("Mag", "Magnitude", names(data))
# BodyBody should be replaced with Body
names(data)<-gsub("BodyBody", "Body", names(data))

# check names
#names(data)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyData <- aggregate(. ~subject + activity, data, mean)
tidyData<-TidyData[order(TidyData$subject,TidyData$activity),]
write.table(TidyData, file = "tidy_data.txt", row.names = FALSE)

library(rmarkdown)
knit2html("codebook.Rmd")



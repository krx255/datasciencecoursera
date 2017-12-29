Coursera: Peer-graded Assignment: 
Getting and Cleaning Data Course Project 

Full description dataset + License available from:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


Data download location
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


Dataset Attribute Information:
===============================

The dataset is then divided in two parts and they can be used separately. 

1. Inertial sensor data 
- Raw triaxial signals from the accelerometer and gyroscope of all the trials with with participants. 
- The labels of all the performed activities. 

2. Records of activity windows. Each one composed of: 
- A 561-feature vector with time and frequency domain variables. 
- Its associated activity label. 
- An identifier of the subject who carried out the experiment. 

The dataset includes the following files: 
========================================= 

- 'README.txt' 

- 'http://archive.ics.uci.edu/ml/datasets/RawData/acc_expXX_userYY.txt': The raw triaxial acceleration signal for the experiment number XX and associated to the user number YY. Every row is one acceleration sample (three axis) captured at a frequency of 50Hz. 

- 'http://archive.ics.uci.edu/ml/datasets/RawData/gyro_expXX_userYY.txt': The raw triaxial angular speed signal for the experiment number XX and associated to the user number YY. Every row is one angular velocity sample (three axis) captured at a frequency of 50Hz. 

- 'http://archive.ics.uci.edu/ml/datasets/RawData/labels.txt': include all the activity labels available for the dataset (1 per row). 
Column 1: experiment number ID, 
Column 2: user number ID, 
Column 3: activity number ID 
Column 4: Label start point (in number of signal log samples (recorded at 50Hz)) 
Column 5: Label end point (in number of signal log samples) 

- 'features_info.txt': Shows information about the variables used on the feature vector. 

- 'features.txt': List of all features. 

- 'activity_labels.txt': Links the activity ID with their activity name. 

- 'http://archive.ics.uci.edu/ml/datasets/Train/X_train.txt': Training set. 

- 'http://archive.ics.uci.edu/ml/datasets/Train/y_train.txt': Training labels. 

- 'http://archive.ics.uci.edu/ml/datasets/Test/X_test.txt': Test set. 

- 'http://archive.ics.uci.edu/ml/datasets/Test/y_test.txt': Test labels. 

- 'http://archive.ics.uci.edu/ml/datasets/Train/subject_id_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'http://archive.ics.uci.edu/ml/datasets/Test/subject_id_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

Notes: 
====== 

- Features are normalized and bounded within [-1,1]. 
- Each feature vector is a row on the 'X' and 'y' files. 
- The units used for the accelerations (total and body) are 'g's (gravity of earth -> 9.80665 m/seg2). 
- The gyroscope units are rad/seg. 
- A video of the experiment including an example of the 6 recorded activities with one of the participants can be seen in the following link: [Web Link] 

###


# Gather Activity files to create activity table
ytrain <- read.table(".\\train\\y_train.txt")
ytest <- read.table(".\\test\\y_test.txt")
activity <- rbind(ytrain, ytest)
names(activity) <- c("activity")


# gather Subject files + set names to create subject table
subjecttrain <- read.table(".\\train\\subject_train.txt")
subjecttest <- read.table(".\\test\\subject_test.txt")
subject <- rbind(subjecttrain, subjecttest)
names(subject) <- c("subject")

# Gather Features files 
xtrain <- read.table(".\\train\\X_train.txt")
xtest <- read.table(".\\test\\X_test.txt")
features <- rbind(xtrain, xtest)

# and... Set names for features-table
FeaturesNames <- read.table(file = "features.txt")
names(features) <- FeaturesNames$V2

# According to the instructions 
# 1. Merge to create one data set.
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

# perhaps easier solution: http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/




# check replacement
#data$activity


# Changes to table names
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

write.table(TidyData, file = "tidy_data.txt", row.names = FALSE)
The output file Tidy_data.txt is a a space-delimited value file with a tidy header



# List of libraries
install.packages("knitr")
install.packages("rmarkdown")
library(rmarkdown)
knit2html("codebook.Rmd")
library(knitr)
library(utils)
require(data.table)
library(dplyr)
## Create R script that does the following.
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Set the working directory


features <- read.table("./features.txt")
activityLabel <- read.table("./activity_labels.txt")

## Reading the Training data
subject_train <- read.table("./train/subject_train.txt")
xTrain <- read.table("./train/X_train.txt")
yTrain <- read.table("./train/y_train.txt")

## Assigning column names

colnames(activityLabel)<-c("activityID","activityType")
colnames(subject_train) <- "subjectID"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityID"

##Merging Training Data
trainingData <- cbind(subject_train,xTrain,yTrain)

##Reading the test Data
subject_Test <-read.table("./test/subject_test.txt")
xTest <- read.table("./test/X_test.txt")
yTest <- read.table("./test/y_test.txt")

## Assign column names 
colnames(subject_Test) <- "subjectID"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityID"

## Merging Testing Data
testingData <- cbind(subject_Test,xTest,yTest)

##Creating one dataset of both
mergedData <- rbind(trainingData,testingData)

## Extract only the measurements on the mean and standard deviation for each measurement

colNames <- colnames(mergedData);
data_mean_std <- mergedData[,grepl("mean|std|subject|activityID",colnames(mergedData))]

library(plyr)

data_mean_std <- join(data_mean_std, activityLabel, by = "activityID", match = "first")
data_mean_std <- data_mean_std[,-1]


names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl  = TRUE)
names(data_mean_std) <- make.names(names(data_mean_std))

## adding descriptive names

names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

## Creates tidy data set with the average of each variable for each activity and each subject.

tidydata_average_sub<- ddply(data_mean_std, c("subjectID","activityType"), numcolwise(mean))

write.table(tidydata_average_sub, file="tidydata.txt")


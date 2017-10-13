# Getting and cleaning the UCI HAR Dataset
library(dplyr)

## Read the feature data and get the position of all mean and std feature
feature <- read.table("UCI HAR Dataset/features.txt")
featureAvail <- grep("mean\\(\\)|std\\(\\)", feature[,2])

## Cleaning test data
### Read the test dataset, extract mean and std feature and rename the column
testData <- read.table("UCI HAR Dataset/test/X_test.txt")
testData <- testData[, featureAvail]
names(testData) <- feature[featureAvail, 2]

### Read the subjects data and activity labels
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
names(testSubject) <- "subject"
testActivity <- read.table("UCI HAR Dataset/test/y_test.txt")
names(testActivity) <- "activity"

### Add the set type, subject num and activity labels to the test data as new columns
testData <- cbind(set = "test", testSubject, testActivity, testData)

## Clean the train data, do the same process
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainData <- trainData[, featureAvail]
names(trainData) <- feature[featureAvail, 2]

trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
names(trainSubject) <- "subject"
trainActivity <- read.table("UCI HAR Dataset/train/y_train.txt")
names(trainActivity) <- "activity"

trainData <- cbind(set = "train", trainSubject, trainActivity, trainData)

## Combine the testData with trainData
data <- rbind(trainData, testData)

## Merge the data with activity labels name
labels <- read.table("UCI HAR Dataset/activity_labels.txt")
names(labels) <- c("activity", "activity_name")
data <- merge(labels, data)
data <- data[,-1]

## Calculate the average of each variables for each activity and subject, and save as data_sum.csv
dataSum <- data %>% select(-set) %>%group_by(activity_name, subject) %>% summarise_all(mean)
write.table(dataSum, "data_sum.txt", row.names = FALSE)
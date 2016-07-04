###Load required packages
library(dplyr)
library(data.table)
library(tidyr)

###setting file path
filesPath <- "C:\\Users\\JIBRAN\\Desktop\\CourseProject\\UCI HAR Dataset"
setwd(filesPath)
###Checking file existance and download from source if not available
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

###Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Read subject files
SubjectTrainData <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
SubjectTestData  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
ActivityTrainData <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
ActivityTestData  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read data files.
TrainingData <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
TestingData  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

mergeSubjectData <- rbind(SubjectTrainData, SubjectTestData)
setnames(mergeSubjectData, "V1", "subject")

mergeActivityData<- rbind(ActivityTrainData, ActivityTestData)
setnames(mergeActivityData, "V1", "activityNum")

dataTable <- rbind(TrainingData, TestingData)

# name variables according to feature 
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
mergeData<- cbind(mergeSubjectData, mergeActivityData)
dataTable <- cbind(mergeData, dataTable)

# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

#Names before
head(str(dataTable),2)
#naming column
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)
#create text file from data table
write.table(dataTable, "TidyData.txt", row.name=FALSE)


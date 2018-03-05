# Getting and Cleaning Data Project - Prakhar Prasad

## Description
Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

## Merges the training and the test sets to create one data set
## Read the dataset into data tables. Following code lines do this job using read.table function

xtrain <-  read.table("./data/UCI HAR Dataset/train/X_train.txt")
ytrain <-  read.table("./data/UCI HAR Dataset/train/y_train.txt")
subjectTrain <-  read.table("./data/UCI HAR Dataset/train/subject_train.txt")

xtest <-  read.table("./data/UCI HAR Dataset/test/X_test.txt")
ytest <-  read.table("./data/UCI HAR Dataset/test/y_test.txt")
subjectTest <-  read.table("./data/UCI HAR Dataset/test/subject_test.txt")

features <- read.table("./data/UCI HAR Dataset/features.txt")
activityLabels = read.table("./data/UCI HAR Dataset/activity_labels.txt")

## Rename the variable names or the column names 
colnames(xtrain) <- features[,2]  
colnames(ytrain) <- "Activity" 
colnames(subjectTrain) <- "Volunteer"

colnames(xtest) <- features[,2]
colnames(ytest) <- "Activity" 
colnames(subjectTest) <- "Volunteer"

## 1. Merges the training and the test sets to create one data set ##
Traindataset <- cbind(ytrain,subjectTrain,xtrain)
Testdataset  <-  cbind(ytest,subjectTest,xtest)
Combinedataset <- rbind(Traindataset,Testdataset)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement by extracting those columns which have the string mean or std
ColNames <- colnames(Combinedataset)
meandataset <- grep("Activity|Volunteer|mean|std", ColNames, value = TRUE)
MeanStd_Dataset <- Combinedataset[,meandataset]

## 3.  Uses descriptive activity names to name the activities in the data set
colnames(activityLabels) <- c("Activity", "ActivityName")
colnames(activityLabels)
Dataset <- merge(MeanStd_Dataset,activityLabels,by = "Activity", all.x = TRUE)

## 4. Appropriately labels the data set with descriptive variable names using gsub.
a <- colnames(Dataset)
a <- gsub("mean","Mean",a)
a <- gsub("std","Standard_Deviation",a)
a <- gsub("[[:punct:]]","",a)
a<-  gsub("Freq","Frequency",a)
a<-  gsub("Mag","Magnitude",a)
colnames(Dataset) <- a

## 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyDataSet <- Dataset
tidyDataSet <- tbl_df(tidyDataSet)
tidyDataSet <- arrange(tidyDataSet,Activity,Volunteer)
tidyDataSet <- aggregate(. ~ Volunteer+Activity,tidyDataSet,mean)
write.table(tidyDataSet,file = "Tidy Data Set.txt",row.names = FALSE)  

## Transformed variable names 
"Volunteer"
 "Activity" 
"tBodyAccMeanX" 
"tBodyAccMeanY" 
"tBodyAccMeanZ" 
"tBodyAccStandardDeviationX" 
"tBodyAccStandardDeviationY" 
"tBodyAccStandardDeviationZ" 
"tGravityAccMeanX" 
"tGravityAccMeanY" 
"tGravityAccMeanZ"
 "tGravityAccStandardDeviationX" 
"tGravityAccStandardDeviationY" 
"tGravityAccStandardDeviationZ"
 "tBodyAccJerkMeanX"
 "tBodyAccJerkMeanY"
 "tBodyAccJerkMeanZ" 
"tBodyAccJerkStandardDeviationX"
 "tBodyAccJerkStandardDeviationY"
 "tBodyAccJerkStandardDeviationZ"
 "tBodyGyroMeanX" "tBodyGyroMeanY" 
"tBodyGyroMeanZ" "tBodyGyroStandardDeviationX
 "tBodyGyroStandardDeviationY" 
"tBodyGyroStandardDeviationZ" 
"tBodyGyroJerkMeanX" 
"tBodyGyroJerkMeanY" 
"tBodyGyroJerkMeanZ" 
"tBodyGyroJerkStandardDeviationX"
 "tBodyGyroJerkStandardDeviationY"
 "tBodyGyroJerkStandardDeviationZ" 
"tBodyAccMagnitudeMean" 
"tBodyAccMagnitudeStandardDeviation" 
"tGravityAccMagnitudeMean" 
"tGravityAccMagnitudeStandardDeviation" 
"tBodyAccJerkMagnitudeMean" 
"tBodyAccJerkMagnitudeStandardDeviation"
 "tBodyGyroMagnitudeMean" 
"tBodyGyroMagnitudeStandardDeviation"
 "tBodyGyroJerkMagnitudeMean"
 "tBodyGyroJerkMagnitudeStandardDeviation" 
"fBodyAccMeanX"
 "fBodyAccMeanY" 
"fBodyAccMeanZ"
 "fBodyAccStandardDeviationX"
 "fBodyAccStandardDeviationY" 
"fBodyAccStandardDeviationZ" 
"fBodyAccMeanFrequencyX"
 "fBodyAccMeanFrequencyY"
 "fBodyAccMeanFrequencyZ"
 "fBodyAccJerkMeanX" 
"fBodyAccJerkMeanY" 
"fBodyAccJerkMeanZ"
 "fBodyAccJerkStandardDeviationX"
 "fBodyAccJerkStandardDeviationY" 
"fBodyAccJerkStandardDeviationZ" 
"fBodyAccJerkMeanFrequencyX"
 "fBodyAccJerkMeanFrequencyY" 
"fBodyAccJerkMeanFrequencyZ" 
"fBodyGyroMeanX" 
"fBodyGyroMeanY"
 "fBodyGyroMeanZ" 
"fBodyGyroStandardDeviationX" 
"fBodyGyroStandardDeviationY" 
"fBodyGyroStandardDeviationZ" 
"fBodyGyroMeanFrequencyX" 
"fBodyGyroMeanFrequencyY"
 "fBodyGyroMeanFrequencyZ"
 "fBodyAccMagnitudeMean" 
"fBodyAccMagnitudeStandardDeviation" 
"fBodyAccMagnitudeMeanFrequency" 
"fBodyBodyAccJerkMagnitudeMean" 
"fBodyBodyAccJerkMagnitudeStandardDeviation" 
"fBodyBodyAccJerkMagnitudeMeanFrequency" 
"fBodyBodyGyroMagnitudeMean" 
"fBodyBodyGyroMagnitudeStandardDeviation"
 "fBodyBodyGyroMagnitudeMeanFrequency"
 "fBodyBodyGyroJerkMagnitudeMean" 

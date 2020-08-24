library(tidyverse)

filezip <- "Coursera_DS3_Final.zip"
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, filezip, method="curl")

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#First, we merge the testing the training datasets into one dataset
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
Total_Data <- cbind(subject_train, Y, X)


#Then, extract only the measurements on the mean and standard deviation for each measurement.
Tidy_Merged_Data <- Total_Data %>% 
                      select(subject, code, contains("mean"), contains("std"))

#Rename the activities in the data set, using descriptive activity names
Tidy_Merged_Data$code <- activity_labels[Tidy_Merged_Data$code, 2]

#Rename the variables in the data set, using descriptive variable names
names(Tidy_Merged_Data)[2] = "activity"
names(Tidy_Merged_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("BodyBody", "Body", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("Mag", "Magnitude", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("^t", "Time", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("^f", "Frequency", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("tBody", "TimeBody", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("-mean()", "Mean", names(Tidy_Merged_Data), ignore.case = TRUE)
names(Tidy_Merged_Data)<-gsub("-std()", "StandardDeviation", names(Tidy_Merged_Data), ignore.case = TRUE)
names(Tidy_Merged_Data)<-gsub("-freq()", "Frequency", names(Tidy_Merged_Data), ignore.case = TRUE)
names(Tidy_Merged_Data)<-gsub("angle", "Angle", names(Tidy_Merged_Data))
names(Tidy_Merged_Data)<-gsub("gravity", "Gravity", names(Tidy_Merged_Data))

#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
FinalData <- Tidy_Merged_Data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Tidy_Merged_Data, "FinalData.txt", row.name=FALSE)




library(dplyr)

#Reading data source from Internet
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename = "Coursera/Dataset.zip"
download.file(url = url, destfile = filename, mode = "wb")
file = unzip(zipfile = filename, exdir = "Coursera")

#Reading downloaded data
features = read.table("Coursera/UCI HAR Dataset/features.txt", col.names = c("n","functions"), stringsAsFactors = F)
activities = read.table("Coursera/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"), stringsAsFactors = F)
subject_test = read.table("Coursera/UCI HAR Dataset/test/subject_test.txt", col.names = "subject", stringsAsFactors = F)
x_test = read.table("Coursera/UCI HAR Dataset/test/X_test.txt", col.names = features$functions, stringsAsFactors = F)
y_test = read.table("Coursera/UCI HAR Dataset/test/y_test.txt", col.names = "code", stringsAsFactors = F)
subject_train = read.table("Coursera/UCI HAR Dataset/train/subject_train.txt", col.names = "subject", stringsAsFactors = F)
x_train = read.table("Coursera/UCI HAR Dataset/train/X_train.txt", col.names = features$functions, stringsAsFactors = F)
y_train = read.table("Coursera/UCI HAR Dataset/train/y_train.txt", col.names = "code", stringsAsFactors = F)

#1.Merges the training and the test sets to create one data set.

merged_x = rbind(x_train, x_test)
merged_y = rbind(y_train, y_test)
merged_subject = rbind(subject_train, subject_test)
merged_data = cbind(merged_subject, merged_y, merged_x)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.

index = grep("mean\\(\\)|std\\(\\)", features[,2])
only.mean.std  =  merged_data[,index]

#3.Uses descriptive activity names to name the activities in the data set

only.mean.std$code = activities[only.mean.std$code,2]

#4.Appropriately labels the data set with descriptive variable name

names(only.mean.std)[2] = "activity"
names(only.mean.std) = gsub("Acc", "Accelerometer", names(only.mean.std))
names(only.mean.std) = gsub("Gyro", "Gyroscope", names(only.mean.std))
names(only.mean.std) = gsub("BodyBody", "Body", names(only.mean.std))
names(only.mean.std) = gsub("Mag", "Magnitude", names(only.mean.std))
names(only.mean.std) = gsub("^t", "Time", names(only.mean.std))
names(only.mean.std) = gsub("^f", "Frequency", names(only.mean.std))
names(only.mean.std) = gsub("tBody", "TimeBody", names(only.mean.std))
names(only.mean.std) = gsub("-mean()", "Mean", names(only.mean.std), ignore.case = TRUE)
names(only.mean.std) = gsub("-std()", "StandardDeviation", names(only.mean.std), ignore.case = TRUE)
names(only.mean.std) = gsub("-freq()", "Frequency", names(only.mean.std), ignore.case = TRUE)
names(only.mean.std) = gsub("angle", "Angle", names(only.mean.std))
names(only.mean.std) = gsub("gravity", "Gravity", names(only.mean.std))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subjec


data.final = aggregate(only.mean.std[,3:66], by = list(activity = only.mean.std$activity, subject = only.mean.std$subject),FUN = mean)

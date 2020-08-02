1. Download the dataset
        Dataset downloaded and extracted under the folder called UCI HAR Dataset

2. Assign each data to variables
        features <- features.txt 
        activities <- activity_labels.txt 
        subject_test <- test/subject_test.txt : 2947 rows, 1 column
        x_test <- test/X_test.txt : 2947 rows, 561 columns
        y_test <- test/y_test.txt : 2947 rows, 1 columns
        subject_train <- test/subject_train.txt : 7352 rows, 1 column
        x_train <- test/X_train.txt : 7352 rows, 561 columns
        y_train <- test/y_train.txt : 7352 rows, 1 columns
        
3. Merges the training and the test sets to create one data set
        merged_x is created by merging x_train and x_test using rbind() function
        merged_y is created by merging y_train and y_test using rbind() function
        merged_subject is created by merging subject_train and subject_test using rbind() function
        merged_data is created by merging merged_x, merged_y and merged_subject using cbind() function

4. Extracts only the measurements on the mean and standard deviation for each measurement
        onlye.mean.std is created by subsetting merged_data, selecting only the measurements on the mean and standard deviation for each measurement

5. Uses descriptive activity names to name the activities in the data set
        Nnumbers in code column of the only.mean.std is replaced with corresponding activity taken from second column of the activities variable

6. Appropriately labels the data set with descriptive variable names
        code column in only.mean.std renamed into activities
        Acc in column’s name replaced by Accelerometer
        Gyro in column’s name replaced by Gyroscope
        BodyBody in column’s name replaced by Body
        Mag in column’s name replaced by Magnitude
        start with character f in column’s name replaced by Frequency
        start with character t in column’s name replaced by Time
        tBody in column’s name replaced by TimeBody
        mean() in column’s name replaced by mean
        freq() in column’s name replaced by Frequency
        angle in column’s name replaced by Angle
	gravity in column’s name replaced by Gravity

7. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
        data.final is created by sumarizing only.mean.std taking the means of each variable for each activity and each subject, after groupped by subject and activity.

8. The result is exported to data_final.txt file in /Coursera directory.

## Getting and Cleaning Data Course Project by Jean-Christophe Prost

0-  Installation of libraries to to facilitate data and table manipulation 
- tidyr
- dplyr
- data.table
 
set up of directories where data shoul be unzip related to R working directory: "~/Coursera/Data science/Working directory/getdata_projectfiles_FUCI HAR Dataset/UCI HAR Dataset"

1-Merges the training and the test sets to create one data set.
 
 
 - In order to read the whole files and upload data in tables, a first step is to create a table named "file_global_info" that contains 
    files names, 
    associated table names,
    directory : computed based on file directory level(l0 = working directory l1 = train or test l2 = inertial_signals), type of data (genric , test or train) , 
    type of file (delimited or fixed with format) + number of columns in case of fixed width format 
    future name table same name as file without extension


-  Loading global data from corresponding directory in one loop based on table previously build.

- File transformations using tbl_df() fonction to facilitate furher manipulation

-  Providing names to tables variable and datacleansing on table Features to avoid character as "(", ")" and ",". Naming columns with calculated value for file except for X_test and X train where colnames cames from features in features file.
Naming "subject y_test and y_train unique column 

- creating global data set with 
        + columns binding tables : y_test, X_test and details for test 
        + columns binding tables : y_train, X_train and details for train
        + Adding an "index" number at each name to avoid duplicate column names (features are not unique)
        + Binding previous two files by rows in one single file

2-Extracts only the measurements on the mean and standard deviation for each measurement by selceting column names that contains "means" or "std


3-Uses descriptive activity names to name the activities in the data set by merging previous table using columns test_label in global file and corresponding column in activity_labels_df file and reorganizing column to have activity number and activity names side by side.


4-Appropriately labels the data set with descriptive variable names. Already done previously for tables . Just renaming activityname and Activityindex


5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

    -create table with the average of eache variable for each subject and activity name

    - write final table to disc in the first level directory define as work_dir




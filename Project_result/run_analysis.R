## Getting and Cleaning Data Course Project by Jean-Christophe Prost

## Installing libraries to manipulate data 
library(tidyr)
library(dplyr)
library(data.table)



##1-Merges the training and the test sets to create one data set.

## directory variables
work_dir <- "~/Coursera/Data science/Working directory/getdata_projectfiles_FUCI HAR Dataset/UCI HAR Dataset"
inertial_signals <- "Inertial Signals"

#File list and associated table list
fil_nam <- c("activity_labels.txt","features.txt",
             "subject_test.txt","y_test.txt","X_test.txt",
             "body_acc_x_test.txt","body_acc_y_test.txt","body_acc_z_test.txt",
             "body_gyro_x_test.txt","body_gyro_y_test.txt","body_gyro_z_test.txt",
             "total_acc_x_test.txt","total_acc_y_test.txt","total_acc_z_test.txt",
             "subject_train.txt","y_train.txt","X_train.txt",
             "body_acc_x_train.txt","body_acc_y_train.txt","body_acc_z_train.txt",
             "body_gyro_x_train.txt","body_gyro_y_train.txt","body_gyro_z_train.txt",
             "total_acc_x_train.txt","total_acc_y_train.txt","total_acc_z_train.txt")
table_list <- NULL
for (i in seq_along (fil_nam)) {
        table_list <- append(table_list,gsub(".txt","",fil_nam[i]))
        }

# Type of file  : d = Delimited  -  fwf = fixed Width format 
file_type <- c("d", "d",
               "d","d","fwf",
               "fwf","fwf","fwf",
               "fwf","fwf","fwf",
               "fwf","fwf","fwf",
               "d","d","fwf",
               "fwf","fwf","fwf",
               "fwf","fwf","fwf",
               "fwf","fwf","fwf")

# Number of columns and length for fixed width format - blank for delimited file
nb <- c("NA","NA",
        "NA","NA",561,
        128,128,128,
        128,128,128,
        128,128,128,
        "NA","NA",561,
        128,128,128,
        128,128,128,
        128,128,128)
wi <-  rep(16,26)

#File directory level : l0 = working directory l1 = train or test l2 = inertial_signals 
file_dir_level <- c("l0","l0",
              "l1","l1","l1",
              "l2","l2","l2",
              "l2","l2","l2",
              "l2","l2","l2",
              "l1","l1","l1",
              "l2","l2","l2",
              "l2","l2","l2",
              "l2","l2","l2")

#type of data : generic , train or test
data_type <- c("generic", "generic",
               "test","test","test",
               "test","test","test",
               "test","test","test",
               "test","test","test",
               "train","train","train",
               "train","train","train",
               "train","train","train",
               "train","train","train")

## File associated directory
file_dir <- NULL 
for (i in seq_along(file_dir_level)){
        if (file_dir_level[i]=="l0") {
               file_dir <- append(file_dir,work_dir)
        } else if (file_dir_level[i]=="l1"){
             file_dir <- append(file_dir,paste(work_dir,data_type[i],sep = "/"))
        } else {file_dir <- append(file_dir,paste(work_dir,data_type[i],inertial_signals,sep = "/"))}
        }

## Build data frame with information to read file into files
file_global_info <- cbind(fil_nam,table_list,file_dir,file_dir_level,file_type,data_type,nb,wi)%>%
                as.data.frame(row.names = 1:26)
file_global_info$nb <- as.numeric(as.character(file_global_info$nb))
file_global_info$wi<- as.numeric(as.character (file_global_info$wi))
file_global_info$table_list <- as.character(file_global_info$table_list)

# Loading global data from corresponding directory in one loop dataframe name is file name without ".txt": 
for (i in seq_along(file_global_info$fil_nam)) {
        if (file_global_info$file_type[i] =="d"){
                assign(file_global_info$table_list[i], 
                read.table(paste(file_global_info$file_dir[i],file_global_info$fil_nam[i], sep = "/"), header = FALSE ,
                           sep = " ", fill = TRUE, strip.white = TRUE ))
        }
        else {
                assign(file_global_info$table_list[i], 
                       read.fwf(paste(file_global_info$file_dir[i],file_global_info$fil_nam[i], sep = "/"), 
                                widths= rep(file_global_info$wi[i],file_global_info$nb[i]), buffersize = 1000 , header = FALSE))
        }
}
#File transformations using tbl_df() fonction to facilitate furher manipulation
for (i in seq_along(file_global_info$table_list)){
        assign (paste(file_global_info$table_list[i],"df",sep = "_") , 
                tbl_df(eval(parse(text = file_global_info$table_list[i]))))
        file_global_info$table_list[i] <- paste(file_global_info$table_list[i],"df",sep = "_")
}  
rm (list = table_list )
## Providing names to tables variable
# Features datacleansing to  avoid character as "(", ")" and ","
colnames(features_df) <- c("index" , "features_name")
features_df$features_name <- as.character(features_df$features_name)
f<- function(x) {gsub("([(),])",replacement = "",x)}
features_df$features_name<-sapply(features_df$features_name,f)

# naming columns with calculated value for file except for X_test where colnames cames from features 
for( i in seq_along(file_global_info$table_list)) {
        if (file_global_info$file_type[i] =="fwf"){
        setnames(get(file_global_info$table_list[i]),paste(sub(file_global_info$data_type[i],replacement = "",
                                file_global_info$table_list[i])                                     
                            ,c(1:file_global_info$nb[i]),sep=""))
                          }
}
# Naming variable table X_test with features from file features
colnames(subject_test_df) <-c("subject")                       
colnames(X_test_df)<-features_df$features_name
colnames(y_test_df)<- c("test_label")
colnames(subject_train_df) <-c("subject")
colnames(X_train_df)<-features_df$features_name
colnames(y_train_df)<- c("test_label")


#creating global data set with all tables 
test <- subject_test_df
train <- subject_train_df
for (i in 4:14 ){
        test <- cbind (test,get(file_global_info$table_list[i]))
        train <- cbind (train,get(file_global_info$table_list[i+12]))
        }

#Indexing the column names to guarantee uniqueness 
for (i in seq_along(names(test))){
        names(test)[i] <- paste(names(test)[i],i, sep = "I")
        names(train)[i] <- paste(names(train)[i],i, sep = "I")
}

global_table <- tbl_df(rbind (test, train)) 


##2-Extracts only the measurements on the mean and standard deviation for each measurement.
global_table_mean_std <- select (global_table,subjectI1,test_labelI2,contains("mean"),contains("std"))

##3-Uses descriptive activity names to name the activities in the data set
global_table_merged <- merge (global_table_mean_std,activity_labels_df,by.x = "test_labelI2", by.y="V1")
global_table_merged_nc <- global_table_merged[,c("subjectI1","test_labelI2","V2",
                                  names(global_table_merged)[names(global_table_merged)!="subjectI1" 
                                        & names(global_table_merged)!="test_labelI2" 
                                        & names(global_table_merged)!="V2"])]


##4-Appropriately labels the data set with descriptive variable names. Already done
global_table_merged_nc <-rename(global_table_merged_nc, activityname = V2, Activityindex = test_labelI2)


##5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
##for each activity and each subject.

#create table with the average of eache variable for each subject by combining tapply in function f and apply
#and  adding column to enditify pivot for mean
f <- function(x) { tapply(x,global_table_merged_nc$subjectI1,mean)}
meanbysubject <- apply(global_table_merged_nc[,4:length(colnames(global_table_merged_nc))],2,FUN = f) %>% tbl_df()
meanbysubject_merged <- left_join(global_table_merged_nc[,1:3],meanbysubject %>% cbind(subjectI1 = c(1:30))%>% tbl_df()
                                  , by= "subjectI1",suffix = c("","bysubjectI1"))%>%
                                 mutate(meanby = "subject")
meanbysubject_merged <-meanbysubject_merged[,c(length(colnames(meanbysubject_merged)),
                                                 1:length(colnames(meanbysubject_merged))-1)]


#create table with the average of eache variable for each activity by combining tapply in function g and apply
#and  adding column to enditify pivot for mean
g <- function(x) { tapply(x,global_table_merged_nc$Activityindex,mean)}
meanbyActivity <- apply(global_table_merged_nc[,4:89],2,FUN = g) %>% tbl_df()
meanbyActivity_merged <- left_join(global_table_merged_nc[,1:3],meanbyActivity %>% cbind(Activityindex = c(1:30))%>% tbl_df()
                                  , by= "Activityindex",suffix = c("","Activityindex"))%>%
                        mutate(meanby = "Activity")
meanbyActivity_merged <-meanbyActivity_merged[,c(length(colnames(meanbyActivity_merged)),
                                                 1:length(colnames(meanbyActivity_merged))-1)]
        


## merging  files to obtain global merged file with index to identify if means are calculated by activity or subject

final_file <- rbind(meanbysubject_merged,meanbyActivity_merged)

## write final table to disc
write.table(final_file, file = paste(work_dir,"final_file.txt",sep = "/"),row.name=FALSE)





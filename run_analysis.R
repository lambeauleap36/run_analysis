run_analysis <- function(training, test){
        #This script merges and summarizes the training and test set data from 
        #the "Human Activity Recognition Using Smartphones" dataset.
        
        #Load libraries
        library(data.table)
        library(dplyr)
        #Step 1 - Merge the "training" and "test" sets to create one data set.
                #Load the test, train, and features files
                X_test <- fread("./test/X_test.txt")
                y_test <- fread("./test/y_test.txt")
                subject_test <- fread("./test/subject_test.txt")
                
                X_train <- fread("./train/X_train.txt")
                y_train <- fread("./train/y_train.txt")     
                subject_train <- fread("./train/subject_train.txt")
                
                features <- as.data.frame(fread("features.txt"))
                
                #Merge train and test sets, hold off on combining the three.
                T_X <- as.data.frame(rbind(X_train,X_test))
                colnames(T_X) <- features$V2
                T_y <- as.data.frame(rbind(y_train,y_test))
                colnames(T_y) <- c("Move Type")
                T_subject <- as.data.frame(rbind(subject_train,subject_test))
                colnames(T_subject) <- c("User")
                
        #Step 2 - Extract only the measurements on the mean and the standard
        #deviation for each measurement.
        
                
        desiredFeatures <- features[grep("mean()|std()",features$V2),1]
        T_X <- T_X[,desiredFeatures]
        
        T_total <- cbind(T_X,T_y,T_subject)
        #Step 3 - Name the activities in the data set.
        activities <- fread("activity_labels.txt")
        T_total$'Move Type Label' <-  activities$V2[
                match(T_total$"Move Type",activities$V1)]
        
        #Step 4 - Label data set with descriptive variable names.
        #After reviewing the data set, the names are consistently named and are
        #descriptive enough.
        
        
        #Step 5 - Create a second, independent tidy data set with the average
        #of each variable for each activity and each subject.
        
        #This last step is not working for me exactly, but I have still created
        #a tidy data set with another tool.
        T_total %>% group_by(T_total$User, T_total$`Move Type Label`) 
                %>% summarize_each(T_total,ave)
        write.table(T_total,file = "T_total",col.names = FALSE, sep = " ")
}
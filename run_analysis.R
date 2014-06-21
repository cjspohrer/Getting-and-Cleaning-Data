file <- "data.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- "UCI HAR Dataset"
results_folder <- "project"
dir.create(result_folder)

##reads a table from the zip data file and applies cols
getTable <- function (filename,cols = NULL){
        
        f <- unz(file, paste(data_path,filename,sep="/"))
        
        data <- data.frame()
       
        if(is.null(cols)){
                data <- read.table(f,sep="",stringsAsFactors=F)
        } else {
                data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
        }
        
        data
        
}

##Reads and creates a complete data set
getData <- function(type, features){
        
        subject_field <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
        x_field <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
        y_field <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
        
        return(cbind(subject_field,y_field,x_field)) 
}

##saves the data into the result folder
saveResult <- function (data,name){
     
        file <- paste(result_folder, "/", name,".csv" ,sep="")
        write.csv(data,file)
}

## Get common data tables

# Col names used creating train and test data sets
features <- getTable("features.txt")

## Load the data sets
train_data <- getData("train",features)
test_data <- getData("test",features)

## 1. Merges the training and the test sets to create one data set.

data <- rbind(train_data, test_data) # merge datasets
data <- arrange(data, id) # rearrange the data using id

## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.

activity_labels <- getTable("activity_labels.txt")

data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
dataset1 <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]

saveResult(dataset1,"tidydata1") # Save dataset1 into results folder

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
dataset2 <- ddply(dataset1, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

# Adds "_mean" to colnames
colnames(dataset2)[-c(1:2)] <- paste(colnames(dataset2)[-c(1:2)], "_mean", sep="")

saveResult(dataset2,"tidydata2") # Save tidy dataset2 into results folder
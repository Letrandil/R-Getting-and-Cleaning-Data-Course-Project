
# Script purpose:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Download and unzip the data from the given source and extract it locally
downloadData = function(url) {

    if (!file.exists("data")) {
        dir.create("data")
    }

    if (!file.exists("data/UCI HAR Dataset")) {
        zipfile="data/UCI_HAR_data.zip"
        download.file(url, destfile=zipfile, method="auto")
        unzip(zipfile, exdir="data")
    }

    message("data download...OK")
}


# Load train and test data 
loadAndMergeData = function() {
    
    path <<- paste(getwd(),"/data/UCI HAR Dataset/", sep = "")

    xTrain <- read.csv(paste(path,"train/X_train.txt",sep=""), sep="", header=FALSE)
    xTrain[,ncol(xTrain)+1] <- read.csv(paste(path,"train/y_train.txt",sep=""), sep="", header=FALSE)
    xTrain[,ncol(xTrain)+1] <- read.csv(paste(path,"train/subject_train.txt",sep=""), sep="", header=FALSE)
    
    xTest <- read.csv(paste(path,"test/X_test.txt",sep=""), sep="", header=FALSE)
    xTest[,ncol(xTest)+1] <- read.csv(paste(path,"test/y_test.txt",sep=""), sep="", header=FALSE)
    xTest[,ncol(xTest)+1] <- read.csv(paste(path,"test/subject_test.txt",sep=""), sep="", header=FALSE)
    
    message("load data...OK")
    
    rbind(xTrain, xTest)
    
}


# Get data mean and std. dev.
extractData = function(df) {

    features <- read.csv(paste(path,"features.txt", sep=""), sep="", header=FALSE)

    scopeColumns <<- grep(".*-mean.*|.*-std.*", features[,2])
    features <<- features[scopeColumns,]
    
    colCount = ncol(df)
    scopeColumns <<- c(scopeColumns, colCount-1, colCount)

    df <- df[,scopeColumns]

    message("extract data...OK")

    df
}

# Appropriately labels the data set with descriptive variable names
descriptiveVariables = function(df){
    
    features[,2] <- gsub("^t", "time", features[,2])
    features[,2] <- gsub("^f", "frequency", features[,2])
    features[,2] <- gsub("Acc", "Accelerometer", features[,2])
    features[,2] <- gsub("Gyro", "Gyroscope", features[,2])
    features[,2] <- gsub("Mag", "Magnitude", features[,2])
    features[,2] <- gsub("BodyBody", "Body", features[,2])
    
    colnames(df) <- c(features$V2, "Activity", "Subject")
    colnames(df) <- tolower(colnames(df))
    message("variable names...OK")

    df
}

# Descriptive activity names to name the activities in the data set
setActivityNames = function(df){
    
    activityLabels = read.csv(paste(path,"activity_labels.txt", sep=""), sep="", header=FALSE)
    activityId = 1
    for (activityLabel in activityLabels$V2) {
        df$activity <- gsub(activityId, activityLabel, df$activity)
        activityId <- activityId + 1
    }
    message("activity labels...OK")
    
    df
}

# Creates tidy data DF with the mean of each variable for each activity and each subject. 
makeTidy = function(df){

    df$activity <- as.factor(df$activity)
    df$subject <- as.factor(df$subject)
    
    countnndc = ncol(df)-2
    nndc = c(1:countnndc)
    
    tidy <- aggregate(df[,nndc], by=list(activity = df$activity, subject=df$subject), mean, na.rm=TRUE)
    message("tidy data...OK")

    tidy
}



# Preparation:
downloadData("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20dfset.zip")

# Merges the training and the test sets to create one data set.
df <- loadAndMergeData()

# Extracts only the measurements on the mean and standard deviation for each measurement. 
df <- extractData(df)

# Appropriately labels the data set with descriptive variable names. 
df <- descriptiveVariables(df)

# Uses descriptive activity names to name the activities in the data set
df <- setActivityNames(df)

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidydf <- makeTidy(df)

write.table(tidydf, "tidydf.txt", sep="\t",row.names = F)


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
    xTest <- read.csv(paste(path,"test/X_test.txt",sep=""), sep="", header=FALSE)
    dataFeatures <- rbind(xTrain, xTest)
    dataFeaturesNames <<- read.csv(paste(path,"features.txt", sep=""), sep="", header=FALSE)
	names(dataFeatures)<- dataFeaturesNames$V2


    yTrain <- read.csv(paste(path,"train/y_train.txt",sep=""), sep="", header=FALSE)
    yTest <- read.csv(paste(path,"test/y_test.txt",sep=""), sep="", header=FALSE)
    dataActivity<- rbind(yTrain, yTest)
    names(dataActivity)<- c("activity")


    sTrain <- read.csv(paste(path,"train/subject_train.txt",sep=""), sep="", header=FALSE)    
    sTest <- read.csv(paste(path,"test/subject_test.txt",sep=""), sep="", header=FALSE)
    dataSubject <- rbind(sTrain, sTest)
	names(dataSubject)<-c("subject")
	
    message("load and merge data...OK")
    dataCombine <- cbind(dataSubject, dataActivity)
	cbind(dataFeatures, dataCombine)
}


# Get data mean and std. dev.
extractData = function(df) {

    subdataFeaturesNames <- dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
    selectedNames <- c(as.character(subdataFeaturesNames), "subject", "activity" )
	subset(df, select=selectedNames)
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

# Appropriately labels the data set with descriptive variable names
descriptiveVariables = function(df){
    
    names(df)<-gsub("^t", "time", names(df))
	names(df)<-gsub("^f", "frequency", names(df))
	names(df)<-gsub("Acc", "Accelerometer", names(df))
	names(df)<-gsub("Gyro", "Gyroscope", names(df))
	names(df)<-gsub("Mag", "Magnitude", names(df))
	names(df)<-gsub("BodyBody", "Body", names(df))

    message("variable names...OK")

    df
}



# Creates tidy data DF with the mean of each variable for each activity and each subject. 
makeTidy = function(df){

    library(plyr);
	tidy <- aggregate(. ~subject + activity, df, mean)
	tidy <- tidy[order(tidy$subject, tidy$activity),]
	
	message("tidy data...OK")

    tidy
}



# Preparation:
downloadData("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20dfset.zip")

# Merges the training and the test sets to create one data set.
df <- loadAndMergeData()

# Extracts only the measurements on the mean and standard deviation for each measurement. 
df <- extractData(df)

# Uses descriptive activity names to name the activities in the data set
df <- setActivityNames(df)


# Appropriately labels the data set with descriptive variable names. 
df <- descriptiveVariables(df)


# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidydf <- makeTidy(df)

write.table(tidydf, "tidydf.txt", sep="\t",row.names = F)
write(names(df), file = "variables.txt", ncolumns = 1)
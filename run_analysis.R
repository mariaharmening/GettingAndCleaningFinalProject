install.packages("reshape2")
library(reshape2)

runAnalysisURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" #URL location of the data
runAnalysisFileName<-"UCIdata.zip" #Name of the zip file

if(!file.exists(runAnalysisFileName))#check to see if the file has already downloaded
{
  download.file(runAnalysisURL,runAnalysisFileName, mode="wb") #file does not yet exist so download the file to the working directory
  unzip(runAnalysisFileName)#unzip
}
#read the text files into dataframes
features<-read.table("UCI HAR Dataset/features.txt", header = FALSE, col.names = c("FeaturesID", "FeatureName")) #has no column names so create them
featuresWanted <- grep("(mean|std)\\(\\)", features[, "FeatureName"]) #determine the indices of the feature names desired within the features.FeatureName values
measurements <- features[featuresWanted, "FeatureName"] #subset the features using the featuresWanted indices
measurements <- gsub('[()]', '', measurements) #remove parenthesis from the desired measurements

activityLabels<-read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("ActivityID", "ActivityName")) #has no column names so create them

xTrain<-read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)[,featuresWanted] #get desired columns from train
colnames(xTrain)<-measurements #rename the columns to the measurements

yTrain<-read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = c("Activity")) #get the activities
subjectTrain<-read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = c("SubjectNumber")) #get the subjects


xTest<-read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)[,featuresWanted] #get desired columns from test
colnames(xTest)<-measurements #rename the columns to the measurements

yTest<-read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = c("Activity")) #get the activities
subjectTest<-read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = c("SubjectNumber")) #get the subjects

mergeTrain<-cbind(subjectTrain, yTrain, xTrain) #column bind the train data
mergeTest<-cbind(subjectTest, yTest, xTest) # column bind the test data

mergedData<-rbind(mergeTrain, mergeTest) #concatenate the train and test data



#associate labels to activities
mergedData[["Activity"]] <- factor(mergedData[, "Activity"], 
                                   levels = activityLabels[["ActivityID"]], labels = activityLabels[["ActivityName"]])

mergedData[["SubjectNumber"]] <- as.factor(mergedData[, "SubjectNumber"]) #make the subject ids into factors

#create second data set
secondTidy <-melt(data = mergedData, id = c("SubjectNumber", "Activity")) #collapse/tag id vars
secondTidy <- dcast(data = secondTidy, SubjectNumber + Activity ~ variable, fun.aggregate = mean) #widen

write.table(secondTidy, "tidydata.txt", sep = ",", row.names=FALSE) #write data to text file

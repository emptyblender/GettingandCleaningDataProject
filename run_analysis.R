library(data.table)
library(dplyr)

##Download file. ################
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(url,destfile="./data/Dataset.zip")


##Unzip file. #####################
unzip(zipfile="./data/Dataset.zip",exdir="./data")

##Create list of files. ################
files<-list.files("./data", recursive=TRUE)

##Read the activity and feature files. ############
features <- read.table("./data/UCI HAR Dataset/features.txt")
activityLabels = read.table("./data/UCI HAR Dataset/activity_labels.txt")


##Read test and train data. ###################
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
dataTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
activityTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
dataTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
activityTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

##change names of columns in activity and subject tables. ###########
setnames(subjectTest, "V1", "subject")
setnames(subjectTrain, "V1", "subject")
setnames(activityTest,"V1", "activity")
setnames(activityTrain,"V1", "activity")


##Merge training and test sets together. ##############
subject<-rbind(subjectTest,subjectTrain)
activity<-rbind(activityTest,activityTrain)
dataTT<-rbind(dataTest,dataTrain)

##name columns of data. ################
colnames(dataTT)<-features[,2]

##1.)  MERGE TEST AND TRAINING SETS TO CREATE ONE DATA SET. #########
##Merge subject,activity,and data sets to create one large, labelled data set. ####
SA<-cbind(subject,activity)
dat<-cbind(SA,dataTT)

##2.) EXTRACT ONLY THE MEASUREMENTS ON MEAN AND STANDARD DEVIATION FOR EACH MEASURE ###
##create subset of features which includes only mean and standard deviation. #############
features[,2] <- as.character(features[,2])
featuresSub<-grep(".*mean.*|.*std.*",features[,2])
featuresSub<-features[featuresSub,2]

##Remove all rows except those with mean and standard deviation. ###################
meanStd<-c("subject","activity",featuresSub)
df<-subset(dat,select=meanStd)

## 3.)USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET ###
##Replace numbers in activity column with proper labels.  ###################
df[,2]<-sub("1","walking",df[,2])
df[,2]<-sub("2","walking_upstairs",df[,2])
df[,2]<-sub("3","walking_downstairs",df[,2])
df[,2]<-sub("4","sitting",df[,2])
df[,2]<-sub("5","standing",df[,2])
df[,2]<-sub("6","laying",df[,2])

## 4.) APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES ###
names(df)<-gsub("^t", "time", names(df))
names(df)<-gsub("^f", "frequency", names(df))
names(df)<-gsub("Gyro", "Gyroscope", names(df))
names(df)<-gsub("Acc", "Accelerometer", names(df))
names(df)<-gsub("BodyBody", "Body", names(df))
names(df)<-gsub("Mag", "Magnitude", names(df))

## 5.) CREATE SECOND INDEPENDENT DATA SET WITH MEAN FOR EACH SUBJECT AND ACTIVITY ###
tidy<-lapply(3:81, function(x) df%>%group_by(subject,activity)%>%summarize(rows=n(),mean(df[,x]))) 
write.table(tidy, file = "tidy.txt",row.name=FALSE)
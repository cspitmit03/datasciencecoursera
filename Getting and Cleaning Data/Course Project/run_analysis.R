##Pure RCode
##Instructions for project
 
#The following is just the preparation of the data before doing any of the work listed in items 1 through 5 for the course project.

##Getting the Data
### 1. download the data directly and unzip all files
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile="./Data.zip",method="curl")
unzip(zipfile="./Data.zip")
path <- file.path("UCI HAR Dataset")

##gives me paths to all files within the main folder
allfiles <- list.files(path, recursive=TRUE)
## the following rows are going to be read in the next section: 
##14 Test Subject Data, 15 Test Feature Data, 16 Test Activity Data
##26 Train Subject Data, 27 Train Feature Data, 28 Train Activity Data

##According to the README file, the data is locatd in the test and train folders.

##Reading the Data
### Read in each data set
library(dplyr)
##read in for test
TestSubject <- read.table(file.path(path,allfiles[14]), header=FALSE)
TestFeatures <- read.table(file.path(path,allfiles[15]), header=FALSE)
TestActivity <- read.table(file.path(path,allfiles[14]), header=FALSE)

##read in for train
TrainSubject <- read.table(file.path(path,allfiles[26]), header=FALSE)
TrainFeatures <- read.table(file.path(path,allfiles[27]), header=FALSE)
TrainActivity <- read.table(file.path(path,allfiles[28]), header=FALSE)

##determining data size
dim(TrainFeatures)

##The feature data sets have 561 columns which matches the features.txt file for naming in the following section

##From this point onward, the items required for this project are listed.

##1. Merges the training and the test sets to create one data set.

##merge the test and train data by type (subject, activity and features)
SubjectAll <- rbind(TrainSubject, TestSubject)
ActivityAll <- rbind(TrainActivity, TestActivity)
FeaturesAll <- rbind(TrainFeatures, TestFeatures)

##See size of data
dim(SubjectAll)
dim(ActivityAll)
dim(FeaturesAll)
##all data is 10299 rows by either 1 or 561 cols

## name columsn prior to making one master table
colnames(SubjectAll) <- c("Subject")
colnames(ActivityAll) <- c("Activity")
featcolnames <- read.table(file.path(path, "features.txt"), header=FALSE)
colnames(FeaturesAll) <- featcolnames$V2

##combine all into one using cbind and throw into Data Frame for dplyr manipulation
AllData <- cbind(SubjectAll, ActivityAll, FeaturesAll)
Data <- tbl_df(AllData)

##2. Extracts only the measurements on the mean and standard deviation for each measurement.

##find column names with mean() or std()
selectcols <- featcolnames$V2[grep("mean\\(\\)|std\\(\\)", featcolnames$V2)]
naming <- c("Subject", "Activity", as.character(selectcols))

##subset data on these cols + Subject and Activity
SmallData <- subset(AllData, select = naming)
MeanStdData <- tbl_df(SmallData)

##3. Uses descriptive activity names to name the activities in the data set

##load activity names
activityNames <- read.table(file.path(path,"activity_labels.txt"), header=FALSE)
colnames(activityNames) <- c("Activity", "ActivityNames")
anames <- tbl_df(activityNames)

##join data from both tables to relabel numerical activity values with actual acitvity names
NewData <- left_join(MeanStdData, anames, by="Activity")

##4. Appropriately labels the data set with descriptive variable names. 

##According to the README file, the following were characters represent specific items
##t = time; substitute all colnames to read time
names(NewData) <- gsub("^t", "time", names(NewData))
##f = frequency; substitute all colnames to read frequency
names(NewData) <- gsub("^f", "frequency", names(NewData))
##Acc = Acceleration / Mag = Magnitude / BodyBody = Body / Gyro = Gyroscope; update all
names(NewData) <- gsub("Acc", "Acceleration", names(NewData))
names(NewData) <- gsub("BodyBody", "Body", names(NewData))
names(NewData) <- gsub("Mag", "Magnitude", names(NewData))
names(NewData) <- gsub("Gyro", "Gyroscope", names(NewData))

##a txt file of the new variable names for the final data set prior to writing it out
Variables <- names(NewData)
write.table(Variables, "Variables.txt", row.name=FALSE)

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Group by Subject and Activity
Grouped <- group_by(NewData, Subject, ActivityNames)
Grouped1 <- mutate_each(Grouped, funs(mean))
Grouped2 <- ungroup(Grouped1)

#ungroup and write data
TidyData <- distinct(Grouped2)
write.table(TidyData, file="tidydata.txt", row.name=FALSE)
write.csv(TidyData, file="tidydata.csv")
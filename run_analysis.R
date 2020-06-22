library("dplyr")
library("data.table")

# download and unzip file
if(!file.exists("Data")){dir.create("Data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "UCI_HAR_Dataset.zip"
path <- getwd()
download.file(url,destfile = paste(path,"/Data","/",fileName,sep = ""))
unzip("./Data/UCI_HAR_Dataset.zip",exdir = "./Data")

# read files
activity_labels <- fread("./Data/UCI HAR Dataset/activity_labels.txt")
features <- fread("./Data/UCI HAR Dataset/features.txt")
X_train <- fread("./Data/UCI HAR Dataset/train/X_train.txt")
Y_train <- fread("./Data/UCI HAR Dataset/train/Y_train.txt")
subject_train <- fread("./Data/UCI HAR Dataset/train/subject_train.txt")
X_test <- fread("./Data/UCI HAR Dataset/test/X_test.txt")
Y_test <- fread("./Data/UCI HAR Dataset/test/Y_test.txt")
subject_test <- fread("./Data/UCI HAR Dataset/test/subject_test.txt")

# complete train set
names(X_train) <- features$V2 
X_train <- cbind(X_train,subject_train$V1)
names(X_train)[length(X_train)] <- "Subject"

X_train <- cbind(X_train,Y_train$V1)
names(X_train)[length(X_train)] <- "Label"

# complete test set
names(X_test) <- features$V2 
X_test <- cbind(X_test,subject_test$V1)
names(X_test)[length(X_test)] <- "Subject"

X_test <- cbind(X_test,Y_test$V1)
names(X_test)[length(X_test)] <- "Label"

# merge train and test set and select the wanted features

dataSet <- rbind(X_train,X_test)
meanStd <- grep(".(mean|std)\\()",names(dataSet),value = TRUE)
dataSet <- select(dataSet,c(meanStd,c("Subject","Label")))
dataSet$Subject <- factor(dataSet$Subject)
dataSet$Label <- factor(dataSet$Label)
write.table(names(dataSet),file = "selectFeatures.txt")

# macth activity in label
matchLabel <- function(el,activity_labels){
  filter(activity_labels,V1 == el)$V2
}
dataSet$Label <- sapply(dataSet$Label,matchLabel,activity_labels)


# independent tidy data set with the average of each variable for each activity and each subject

tidyData <- dataSet %>% group_by(Label,Subject) %>% summarise_each(funs = mean)
write.table(tidyData,"./Data/tidyData.txt")
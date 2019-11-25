
library(tidyverse)
library(data.table)
library(stringr)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","dataset_final_assessment.zip")
unzip(zipfile = "dataset_final_assessment.zip") #we unzip the folder

train_ds<-fread(file = "UCI HAR Dataset/train/X_train.txt")# the train dataset
test_ds <-fread(file = "UCI HAR Dataset/test/X_test.txt")#the test dataset read as a data.table
feats<- fread(file="UCI HAR Dataset/features.txt")

names(train_ds)<-unlist(feats[,2]) #we name the two datasets using the feature file containing the names of the columns, we do that for both datasets
names(test_ds)<-unlist(feats[,2])
y_train <-fread("UCI HAR Dataset/train/y_train.txt")#labels for the rows of the trian set
y_test <- fread("UCI HAR Dataset/test/y_test.txt")#labels for the rows of the test set
train_ds<-bind_cols(train_ds,y_train)#we attach the activity infos to the train_ds
train_ds <- train_ds[,c(562,1:561)]
test_ds <- bind_cols(test_ds,y_test)
test_ds <- test_ds[,c(562,1:561)]#we rearrange the cols to have the activity label as a first row
traintest<-bind_rows(test_ds,train_ds)#we merge the 2 data sets
traintest_meanstd<-traintest%>%#we select only the cols with a mean or a standard deviation and the activity column
  select(matches('V1|mean()|std()',ignore.case = FALSE))

act_labs <- fread("UCI HAR Dataset/activity_labels.txt")#the labels for the activities
names(traintest_meanstd)[1]<- "activity" #we change the name of the activity column 
traintest_meanstd$activity <- as.factor(traintest_meanstd$activity) #transform the activity col in a factor
levels(traintest_meanstd$activity)<-act_labs$V2 #we replace the levels of the activity variable
names(traintest_meanstd)<-names(traintest_meanstd)%>% #we remove the parenthesis, the dashes are replace by an underscore and all the col names are set to lower cases
  str_remove(pattern = "\\(\\)")%>%
  str_replace_all(pattern = "\\-",replacement = "_")%>%
  tolower()

traintest_meanstd_sum<- traintest_meanstd %>% #we group by activity and summarize all the columns
  group_by(activity)%>% 
  summarise_all(mean)

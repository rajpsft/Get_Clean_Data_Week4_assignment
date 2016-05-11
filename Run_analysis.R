#This R Script does the folowing things:

#0:get data
#1.Merges the training and the test sets to create one data set.

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 

#3.Uses descriptive activity names to name the activities in the data set.

#4.Appropriately labels the data set with descriptive variable names. 

#5.From the data set in step 4, creates a second, independent tidy data


#  set with the average of each variable for each activity and each subject.
#
#
# Reading Data into R
#
data_train_x<-read.table("./UCI HAR Dataset/train/X_train.txt")

data_test_x<-read.table("./UCI HAR Dataset/test/X_test.txt")

lbl_train_y<-read.table("./UCI HAR Dataset/train/y_train.txt")

lbl_test_y<-read.table("./UCI HAR Dataset/test/y_test.txt")

sub_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

sub_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

#1:Merges the training and the test sets

data_merge<-rbind(data_train_x,data_test_x)

lbl_merge<-rbind(lbl_train_y,lbl_test_y)

sub_merge<-rbind(sub_train,sub_test)


#2.Extracts only the measurements on the mean and standard deviation

feature_names<-read.table("./UCI HAR Dataset/features.txt")

colnames(data_merge)<-c(as.character(feature_names[,2]))

mean_list<-grep("mean()",colnames(data_merge),fixed=T)

std_list<-grep("std()",colnames(data_merge),fixed=T)

data_extr<-data_merge[,c(mean_list,std_list)]

#3.Uses descriptive activity names to name the activities

activity_merge<-cbind(lbl_merge,data_extr)

colnames(activity_merge)[1]<-"Activity"

#4.Appropriately labels the data set with descriptive variable names

lbl_activity<-read.table("./UCI HAR Dataset/activity_labels.txt")

lbl_activity[,2]<-as.character(lbl_activity[,2])

for(i in 1:length(activity_merge[,1])){
  activity_merge[i,1]<-lbl_activity[activity_merge[i,1],2]
}

#5.creates a second, independent tidy data set

data_final<-cbind(sub_merge,activity_merge)

colnames(data_final)[1]<-"Subject"

data_tidy<-aggregate(data_final[,3]~Subject+Activity,data = data_final,FUN = mean)

for(i in 4:ncol(data_final)){
  data_tidy[,i]<-aggregate(data_final[,i]~Subject+Activity,data = data_final,FUN = mean)[,3]
}
colnames(data_tidy)[3:ncol(data_final)]<-colnames(data_extr)

write.table(data_tidy,file = "result_data.txt",row.name=F)

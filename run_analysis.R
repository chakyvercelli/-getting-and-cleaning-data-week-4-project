
#Read features.txt
features_list<- read.table("C:/Users/chaky/OneDrive/Desktop/Data Science/Getting and Cleaning data/Week 4/UCI HAR Dataset/features.txt")

#Assing column number 2 of "features_list" to "feature_vector"
feature_vector<- features_list[,"V2"]

#Read in X_train and assign column labels from 'features.txt' file
#X_train <- read.table("X_train.txt", col.names=scan('features.txt', what ="", sep = "\t", quiet = TRUE),check.names = FALSE)

#Read in X_train and assign column labels from 'feature_vector'.
X_train <- read.table("C:/Users/chaky/OneDrive/Desktop/Data Science/Getting and Cleaning data/Week 4/UCI HAR Dataset/train/X_train.txt", col.names=feature_vector,check.names = FALSE)

#Read in y_train and assign column label "Activity"
y_train <- read.table("C:/Users/chaky/OneDrive/Desktop/Data Science/Getting and Cleaning data/Week 4/UCI HAR Dataset/train/Y_train.txt", col.names="Activity")

#Read in subject_train and assign column label "Subject"
subject_train <- read.table("C:/Users/chaky/OneDrive/Desktop/Data Science/Getting and Cleaning data/Week 4/UCI HAR Dataset/train/subject_train.txt", col.names="Subject")

#Read in X_test and assign column labels from 'features.txt' file
#X_test <- read.table("X_test.txt", col.names=scan('features.txt', what ="", sep = "\t", quiet = TRUE),check.names = FALSE)

#Read in X_test and assign column labels from 'feature_vector'.
X_test <- read.table("C:/Users/chaky/OneDrive/Desktop/Data Science/Getting and Cleaning data/Week 4/UCI HAR Dataset/test/X_test.txt", col.names=feature_vector, check.names=FALSE)

#Read in Y_test and assign column labels from 'Activity'.
y_test <- read.table("C:/Users/chaky/OneDrive/Desktop/Data Science/Getting and Cleaning data/Week 4/UCI HAR Dataset/test/Y_test.txt", col.names="Activity")

#Read in subject_test and assign column labels from 'Subject'.
subject_test <- read.table("C:/Users/chaky/OneDrive/Desktop/Data Science/Getting and Cleaning data/Week 4/UCI HAR Dataset/test/subject_test.txt", col.names="Subject")

#Combine X_train and X_test to form df
df <- rbind.data.frame(X_train,X_test)

#Combine y_train and y_test to "Activity" column
activity <- rbind.data.frame(y_train,y_test)

#Combine subject_train and subject_test to "Subject" column
subject <- rbind.data.frame(subject_train,subject_test)

#Select columns that only contain means and standard deviations.
#The grep pattern is "mean\\(\\)" and "std\\(\\)"

#Select column names with means.
df1 <- df[ , grep("mean\\(\\)", names(df), perl = TRUE ) ]

#Select column names with standard deviations.
df2 <- df[ , grep("std\\(\\)", names(df), perl = TRUE ) ]

#Combine the two dataframe and place the columns in alphabetical order.
final_df <- cbind(df1,df2)
final_df <- final_df[,order(names(final_df))]

#Add activity and subject vectors to final_df
final_df <- cbind(activity,subject,final_df)

#Adjust typo for columns with "BodyBody" in their name.
names(final_df) <- gsub("BodyBody", "Body", names(final_df), fixed = TRUE)

#Uncode the activities in the "Activities" column
final_df$Activity[which(final_df$Activity == 1)] ="WALKING"
final_df$Activity[which(final_df$Activity == 2)] ="WALKING_UPSTAIRS"
final_df$Activity[which(final_df$Activity == 3)] ="WALKING_DOWNSTAIRS"
final_df$Activity[which(final_df$Activity == 4)] ="SITTING"
final_df$Activity[which(final_df$Activity == 5)] ="STANDING"
final_df$Activity[which(final_df$Activity == 6)] ="LAYING"


library(dplyr)

#Sort final_df by Activity and Subject
final_df <- final_df[order(final_df$Subject, final_df$Activity),]

#Calculate mean values for each of the columns
means <- suppressWarnings(aggregate(final_df,by = list(final_df$Activity,final_df$Subject),function (x) mean(as.numeric(as.character(x)))))

#Clean up columns in the "means" dataframe.
means <- subset(means, select=-c(Activity,Subject))
names(means)[names(means)=="Group.1"] <- "Activity"
names(means)[names(means)=="Group.2"] <- "Subject"

#To create 'tidy' data, place "Subject" column at beginning of dataframe.
means <- means %>% select(Subject, everything())
write.table(means,file="tidy_data.txt",row.name=FALSE)
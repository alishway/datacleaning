###################################################
# About the provided files
# - 'features_info.txt': Shows information about the variables used on the feature vector.
# - 'features.txt': List of all features.
# - 'activity_labels.txt': Links the class labels with their activity name.
# - 'train/X_train.txt': Training set.
# - 'train/y_train.txt': Training labels.
# - 'test/X_test.txt': Test set.
# - 'test/y_test.txt': Test labels.
###################################################

library(reshape2)

## Task 1 - Merge the training and the test sets to create one data set.

# reading the test and training sets' components (activity, measurements and subject)
# the files are space delimited, which is the default option in the read.table function. we can call the function without any additional parameters.
# combine X test and training set first
x_test_set<-read.table("UCI HAR Dataset/test/X_test.txt")
x_training_set<- read.table("UCI HAR Dataset/train/X_train.txt")
# load the features name 
features_names <- read.table("UCI HAR Dataset/features.txt",strip.white=TRUE, stringsAsFactors=FALSE)
features_names_list <- as.vector(as.matrix(features_names$V2))
# rename both X test and training set
names(x_training_set) <- features_names_list
names(x_test_set) <- features_names_list
# Combine X test and training set
x_complete <- rbind(x_training_set, x_test_set)

# next combine Y test and training sets
# read Y test set
y_test_set <- read.table("UCI HAR Dataset/test/Y_test.txt")
# read Y training set
y_training_set <- read.table("UCI HAR Dataset/train/Y_train.txt")
# rename labels for both Y test and training set
names(y_training_set) <- c("activity")
names(y_test_set) <- c("activity")
# Combine Y test and training set
y_complete <- rbind(y_training_set, y_test_set)

# next combine subjects for both test and training sets
subject_training_set <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test_set <- read.table("UCI HAR Dataset/test/subject_test.txt")
# rename labels
names(subject_training_set) <- c("subjects")
names(subject_test_set) <- c("subjects")

# Combine subjects test and training sets
subject_complete <- rbind(subject_training_set, subject_test_set)






# Combine all data together
dataset <- cbind(x_complete,y_complete,subject_complete)
# to view a snapshot of dataset
dataset[1:5, 1:5] # we only take a look at 5 rows and columns of the dataset 

  # tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y
# 1         0.2885845       -0.02029417        -0.1329051       -0.9952786       -0.9831106
# 2         0.2784188       -0.01641057        -0.1235202       -0.9982453       -0.9753002
# 3         0.2796531       -0.01946716        -0.1134617       -0.9953796       -0.9671870
# 4         0.2791739       -0.02620065        -0.1232826       -0.9960915       -0.9834027
# 5         0.2766288       -0.01656965        -0.1153619       -0.9981386       -0.9808173


# Tasks 2. Select only the measurements on the mean and standard deviation
#features_mean_stdx <- features_names[grep("mean\\(\\)|std\\(\\)", features_names_list), ]
#features_mean_std <- regexpr("*mean\\(\\)*",features_names_list)>0 | regexpr("*std\\(\\)*",features_names_list)>0
	features_mean_std <- regexpr("*mean\\(\\)*|*std\\(\\)*",features_names_list)>0 
	index_mean_std <- cbind(x_complete[,features_mean_std],y_complete,subject_complete)
# head(index_mean_std)
 # tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z
# 1         0.2885845       -0.02029417        -0.1329051       -0.9952786       -0.9831106       -0.9135264
# 2         0.2784188       -0.01641057        -0.1235202       -0.9982453       -0.9753002       -0.9603220
# 3         0.2796531       -0.01946716        -0.1134617       -0.9953796       -0.9671870       -0.9789440
# 4         0.2791739       -0.02620065        -0.1232826       -0.9960915       -0.9834027       -0.9906751
# 5         0.2766288       -0.01656965        -0.1153619       -0.9981386       -0.9808173       -0.9904816
# 6         0.2771988       -0.01009785        -0.1051373       -0.9973350       -0.9904868       -0.9954200	

#Task 3 - replace the name of the activities in the data set with more meaningful name
	# already label this above
	
#Task 4 - appropriately label the data set
# Remove occurances of -mean(), -std(), and other formations of them

 names(index_mean_std) <- gsub("\\-mean\\(\\)\\-", "-mean.", names(index_mean_std))
 names(index_mean_std) <- gsub("\\-std\\(\\)\\-", "-standard_deviation.", names(index_mean_std))
 names(index_mean_std) <- gsub("\\-mean\\(\\)", "-mean.", names(index_mean_std))
 names(index_mean_std) <- gsub("\\-std\\(\\)", "-standard_deviation.", names(index_mean_std))
 head(index_mean_std)

#Task 5 - Tidy data

tidyx<-aggregate(activity ~ ., FUN = mean, data=index_mean_std)
write.table(tidyx, file="tidy_data.txt", sep=",", row.names = TRUE, col.names = TRUE)

	
# 1. Merges the training and the test sets to create one data set. 
X_train <- read.table("./UCI_HAR_Dataset/train/X_train.txt")
Y_train <- read.table("./UCI_HAR_Dataset/train/y_train.txt")
X_test <- read.table("./UCI_HAR_Dataset/test/X_test.txt")
Y_test <- read.table("./UCI_HAR_Dataset/test/y_test.txt") 
Subject_train <- read.table("./UCI_HAR_Dataset/train/subject_train.txt")
Subject_test <- read.table("./UCI_HAR_Dataset/test/subject_test.txt")
 
dim(X_train) 
head(X_train)
table(Y_train)
dim(X_test) 
table(Y_test) 

CombinedData <- rbind(X_train, X_test)
CombinedLbl <- rbind(Y_train, Y_test)
CombinedSub <- rbind(Subject_train, Subject_test)

dim(CombinedData)
dim(CombinedLbl) 
dim(CombinedSub) 

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
 
FeaturesData <- read.table("./UCI_HAR_Dataset/features.txt")
MeanStdData <- grep("mean\\(\\)|std\\(\\)", FeaturesData[, 2])

dim(FeaturesData)  
length(MeanStdData)

CombinedData <- CombinedData[, MeanStdData]
dim(CombinedData) 
names(CombinedData) <- gsub("\\(\\)", "", FeaturesData[MeanStdData, 2]) 
names(CombinedData) <- gsub("mean", "Mean", names(CombinedData)) 
names(CombinedData) <- gsub("std", "Std", names(CombinedData)) 
names(CombinedData) <- gsub("-", "", names(CombinedData)) 

# 3. Uses descriptive activity names to name the activities in the data set

Activity_Label <- read.table("./UCI_HAR_Dataset/activity_labels.txt")
Activity_Label[, 2] <- tolower(gsub("_", "", Activity_Label[, 2]))
substr(Activity_Label[2, 2], 8, 8) <- toupper(substr(Activity_Label[2, 2], 8, 8))
substr(Activity_Label[3, 2], 8, 8) <- toupper(substr(Activity_Label[3, 2], 8, 8))
ActivityCombineLbl <- Activity_Label[CombinedLbl[, 1], 2]
CombinedLbl[, 1] <- ActivityCombineLbl
names(CombinedLbl) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names.
  
names(CombinedSub) <- "subject"
tidyData <- cbind(CombinedSub, CombinedLbl, CombinedData)
dim(tidyData) 
write.table(tidyData, "tidy_dataset.csv") 

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
 
subjectLen <- length(table(CombinedSub)) 
activityLen <- dim(Activity_Label)[1] 
columnLen <- dim(tidyData)[2]
tidyMeanData <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
tidyMeanData <- as.data.frame(tidyMeanData)
colnames(tidyMeanData) <- colnames(tidyData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    tidyMeanData[row, 1] <- sort(unique(CombinedSub)[, 1])[i]
    tidyMeanData[row, 2] <- Activity_Label[j, 2]
    bool1 <- i == tidyData$subject
    bool2 <- Activity_Label[j, 2] == tidyData$Activity_Label
    tidyMeanData[row, 3:columnLen] <- colMeans(tidyData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(tidyMeanData)
write.table(tidyMeanData, "tidy_mean_dataset.csv") 

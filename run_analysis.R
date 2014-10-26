# Load & combine training & test sets
test <- read.table("./test/X_test.txt", header=FALSE)
train <- read.table("./train/X_train.txt", header=FALSE)
fullset <- rbind(train,test)

# Add feature names to combined dataset
features <- read.table("features.txt", header=FALSE)
colnames(fullset) <- features[,2]

# Add activity name column to dataset
test_y <- read.table("./test/y_test.txt", header=FALSE)
train_y <- read.table("./train/y_train.txt", header=FALSE)
full_y <- rbind(train_y,test_y)
fullset["Activity"] <- full_y
fullset$Activity <- sub(1,"WALKING",fullset$Activity)
fullset$Activity <- sub(2,"WALKING_UPSTAIRS",fullset$Activity)
fullset$Activity <- sub(3,"WALKING_DOWNSTAIRS",fullset$Activity)
fullset$Activity <- sub(4,"SITTING",fullset$Activity)
fullset$Activity <- sub(5,"WALKING_STANDING",fullset$Activity)
fullset$Activity <- sub(6,"LAYING",fullset$Activity)

# Add subject ID column to dataset
test_subject <- read.table("./test/subject_test.txt", header=FALSE)
train_subject <- read.table("./train/subject_train.txt", header=FALSE)
full_subject <- rbind(train_subject,test_subject)
fullset["SubjectID"] <- full_subject

# Pull out only means & std devs (and some reordering)
subset <- fullset[,c(563,562,1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,
                     121,122,123,124,125,126,161,162,163,164,165,166,201,202,
                     214,215,227,228,240,241,253,254,266,267,268,269,270,271,
                     345,346,347,348,349,350,424,425,426,427,428,429,
                     503,504,516,517,529,530,542,543)]

# Calculate averages per subject, per activity, for each measurement variable
newset <- data.frame(Activity = c("LAYING","SITTING","WALKING","WALKING_DOWNSTAIRS",
                       "WALKING_STANDING","WALKING_UPSTAIRS"))
for (ID in 1:30) {
        newset1 <- data.frame(Activity = c("LAYING","SITTING","WALKING","WALKING_DOWNSTAIRS",
                                          "WALKING_STANDING","WALKING_UPSTAIRS"))
        newset1["SubjectID"] <- ID
        subset1 <- subset[subset$SubjectID==ID,]  # pull out subject with given ID
        for (var in 3:68){
                varvalues <- as.numeric(subset1[,var])
                output <- data.frame(tapply(varvalues,subset1$Activity,mean))  # calculate mean per activity
                newset1[colnames(subset1[var])] <- as.numeric(output[,1])
        }
        if (ID==1){
                newset <- newset1
        } else {
                newset <- rbind(newset,newset1)
        }
}

write.table(newset,"wearables.txt",row.names=FALSE)

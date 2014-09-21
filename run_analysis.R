# Getting and Cleaning Data: Course project
#####
nrows = 10000 # For test purposes
## Read column names
column_names <- read.table('UCI_HAR_Dataset/features.txt',
                        col.names=c('number', 'feature'))
## load the raw data
trainset <- read.table('UCI_HAR_Dataset/train/X_train.txt', nrows = nrows,
                    row.names = NULL, col.names=column_names$feature)
testset <- read.table('UCI_HAR_Dataset/test/X_test.txt', nrows = nrows,
                   row.names = NULL, col.names=column_names$feature)
## Each entry in each table is associated with a specific subject. The number
## identifying the subject can be located on a file called subject_***.txt,
## where the asterisks mean 'train' and 'test'
### Reads subject and adds to main tables
trainset$subject <- read.table('UCI_HAR_Dataset/train/subject_train.txt',
                            nrows=nrows)$V1
testset$subject <- read.table('UCI_HAR_Dataset/test/subject_test.txt',
                           nrows=nrows)$V1
## Each entry in each table is associated with a specific label. The labels
## identifiers are stored on the y_train.txt and y_test_txt file. The file
## activity_labels.txt store the name of each label.
### Reads the label identification
activity_labels <- read.table('UCI_HAR_Dataset/activity_labels.txt',
                          col.names=c('number', 'activity'))
### read labels for each table
label_train <- read.table('UCI_HAR_Dataset/train/y_train.txt',
                          nrows=nrows)
label_test <- read.table('UCI_HAR_Dataset/test/y_test.txt',
                         nrows=nrows)
### Adds to main table
trainset$activity <- activity_labels$activity[label_train$V1]
testset$activity <- activity_labels$activity[label_test$V1]
## Merge datasets
merged_datasets <- rbind(trainset, testset)
## Save merged dataset
write.csv(merged_datasets, 'UCI_HAR_Dataset/output/full_dataset.txt', row.names=FALSE)
################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each
# measurement.
## Get the names of the columns with measurement of mean or standard deviation
pattern <- '[Mm]ean|[Ss]td'
mean_std_cols <- names(merged_datasets)[grep(pattern, names(merged_datasets))]
## Save the name of the columns used
write.table(mean_std_cols, 'UCI_HAR_Dataset/output/features.txt', quote=FALSE)
## Subset the data with the mean_std_cols + `subject` and `activity`
mean_std_dataset <- merged_datasets[c(mean_std_cols, 'subject', 'activity')]
################################################################################
# 3. Uses descriptive activity names to name the activities in the data set.
## This has been done on step 1.
################################################################################
# 4. Appropriately labels the data set with descriptive variable names.
## This has already been done when I load the raw data. The columns names are
## the same as the `features.txt` file.
################################################################################
# 5. Creates a second, independent tidy data set with the average of each
# variable for each activity and each subject.
## Creates a iterator with subjects and activities
iterator <- expand.grid(subject=unique(merged_datasets$subject),
                        activity=activity_labels$activity)
iterator$activity <- as.character(iterator$activity)

### sort by subject
iterator <- iterator[order(iterator[,1]), ]

## Set the names for the rows. These are the subject + activity
merge_names <- function(subject, activity){
  gsub('_', '.', paste(subject, tolower(activity), sep='#'))
}
row_names <- apply(iterator, 1, function(x ,y) merge_names(x[1], x[2]))
## Creates a function that calculates the average for one subject and one
## activity
average <- function(subject, activity){
  subject <- as.numeric(subject) # If not, it gives NA for <10
  flag <- ifelse(mean_std_dataset$subject == subject &
                   mean_std_dataset$activity == activity,
                 TRUE, FALSE)
  stats <- colMeans(mean_std_dataset[flag,][mean_std_cols])
  stats
}
## Creates de tidy data set
tidy_data <- data.frame(apply(iterator, 1, function(x ,y) average(x[1], x[2])))
tidy_data <- t(tidy_data)
row.names(tidy_data) <- row_names
## Saves the tiydy data set
write.table(tidy_data, 'UCI_HAR_Dataset/output/tidy_dataset.txt',row.name=FALSE)

run_analysis <- function (){
    # Extracts and tidies data from the study of human activity
    # using the smartphone accelerometer:
    # (Requires data to be downloaded into current folder, under './data/')    
    
    # Read in data and use features as column names
    features <- read.table('./UCI HAR Dataset/features.txt')[[2]]
    train_data <- read.table('./UCI HAR Dataset/train/X_train.txt', col.names = features)
    test_data <- read.table('./UCI HAR Dataset/test/X_test.txt', col.names = features)
    
    # Attach activity and subject labels on left:
    train_data <- cbind(read.table('./UCI HAR Dataset/train/y_train.txt', col.names = 'activity'), train_data)
    test_data <- cbind(read.table('./UCI HAR Dataset/test/y_test.txt', col.names = 'activity'), test_data)
    train_data <- cbind(read.table('./UCI HAR Dataset/train/subject_train.txt', col.names = 'subject'), train_data) 
    test_data <- cbind(read.table('./UCI HAR Dataset/test/subject_test.txt', col.names = 'subject'), test_data) 
    
    # Merge datasets:
    merged_data <- full_join(train_data, test_data)
    
    # Select mean /std measurements
    merged_select <- merged_data[grep('subj|act|mean|std', names(merged_data), value=TRUE)]
    
    # Rename activity markers with descriptive labels
    labels <- read.table('./data/activity_labels.txt')
    labeled <- mutate(merged_select, activity = labels[activity, 2])  
    names(labeled) <- gsub('*\\.', '', names(labeled))
    
    # Create separate tidy dataset
    tidy <- aggregate(.~ subject+activity, data=labeled, FUN='mean')
    write.table(tidy, './data/accelerometer_means.txt', row.names = FALSE)
    return(tidy)
}
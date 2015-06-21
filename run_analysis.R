downloadData <- function() {
    
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    
    projectdir <- "getdata-015/project"
    
    datasetfile <- paste0(projectdir, "/data.zip")
    
    if (!file.exists(datasetfile)) { 
        
        download.file(url, destfile = datasetfile, method = "curl")
        
    }
    
    datasetdir <- paste0(projectdir, "/UCI HAR Dataset")
    
    if (!file.exists(datasetdir)) {
        
        unzip(datasetfile, exdir = ".")
        
    }
    
    datasetdir
    
}


run_analysis <- function() {
    
    library(dplyr)
    
    datasetdir <- downloadData()
    
    metadata <- loadMetadata(datasetdir)
    
    test_file <- paste0(datasetdir, "/test/X_test.txt")
    train_file <- paste0(datasetdir, "/train/X_train.txt")
    
    X_test <- read.table(test_file)
    X_train <- read.table(train_file)
    X_test <- mutate(X_test, set = "test")
    X_train <- mutate(X_train, set = "train")
    
    features <- metadata[["features"]][,2]
    selected_features <- features[grep("(mean|std)\\(\\)", features)]
    
    data <- rbind(X_test, X_train)
    names(data) <- c(features, "set")
    
    data <- cbind(data[,selected_features], set = data[,"set"])
    
    subjects <- c(metadata[["subject_test"]][,1], metadata[["subject_train"]][,1])
    activity_id <- c(metadata[["y_test"]][,1], metadata[["y_train"]][,1])
    activitys <- metadata[["activity_labels"]]
    names(activitys) <- c("id","label")
    
    data <- mutate(data, subject = subjects, activity_id = activity_id)
    
    data <- merge(data, activity_labels, by.x = "activity_id", by.y = "id")
    
    data <- rename(data, activity = label)
    
    data <- select(data, -set)
    
    data <- select(data, -activity_id)
    
    summdata <- aggregate(. ~ activity + subject, data = data, mean)
    
    write.table(summdata, "tidyMeans.txt", row.names = FALSE)
    
    summdata
    
}

loadMetadata <- function(datadir) {
    
    features_file <- paste0(datadir, "/features.txt")
    
    activity_labels_file <- paste0(datadir, "/activity_labels.txt")
    
    train_dir <- paste0(datadir, "/train")
    
    test_dir <- paste0(datadir, "/test")
    
    features <- read.csv(features_file, sep = " ", stringsAsFactors = FALSE, header = FALSE)
    
    activity_labels <- read.csv(activity_labels_file, sep = " ", stringsAsFactors = FALSE, header = FALSE)
    
    y_train_file <- paste0(train_dir, "/y_train.txt")
    
    subject_train_file <- paste0(train_dir, "/subject_train.txt")
    
    y_test_file <- paste0(test_dir, "/y_test.txt")
    
    subject_test_file <- paste0(test_dir, "/subject_test.txt")
    
    y_train <- read.csv(y_train_file, header = FALSE)
    
    subject_train <- read.csv(subject_train_file, header = FALSE)
    
    y_test <- read.csv(y_test_file, header = FALSE)
    
    subject_test <- read.csv(subject_test_file, header = FALSE)
    
    list(features = features, activity_labels = activity_labels, 
         y_train = y_train, subject_train = subject_train,
         y_test = y_test, subject_test = subject_test)
    
}
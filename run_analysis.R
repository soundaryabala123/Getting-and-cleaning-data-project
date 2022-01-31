# Import modules
library(stringr)
library(dplyr)

# Set current working dir to dir, there this script is located
scriptPath <- function() {
    getSrcDirectory(scriptPath);
}
setwd(scriptPath())

# Path to dataset:
dataset_path = paste(getwd(), "UCI HAR Dataset", sep="/")

download_data = function() {
    url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipfile = str_c(getwd(), "/", "getdata_projectfiles_UCI_HAR_Dataset.zip")
    print("Downloading data file")
    download.file(url, zipfile, method="curl")
    print("Unzipping data file")
    unzip(zipfile)
    print("Dataset successfully downloaded and extracted")
}

if (!dir.exists(str_c(getwd(), "/", "UCI HAR Dataset")))
{
    print("Dataset not found, so, download it and extract...")
    download_data()
} else {
    print("Dataset already downloaded")
}

# Read features:
features = read.table(
    paste(dataset_path, "features.txt", sep="/"), 
    header=FALSE, col.names=c("num", "feature")
)

# Function, that make features names more descriptive
make_features_descriptive = function(features){
    f = gsub("^t", "Time_", features[, "feature"])
    f = gsub("^f", "Frequency_", f)
    f = gsub("Acc", "Acceleration_", f)
    f = gsub("Gyro", "Gyroscope_", f)
    f = gsub("Jerk", "Jerk_", f)
    f = gsub("BodyBody", "Body", f)
    f = gsub("Body", "Body_", f)
    f = gsub("Mag", "Magnitude_", f)
    f = gsub("[-()]+", "_", f)
    f = gsub("__", "_", f)
    return (f)
}

features_descriptive = make_features_descriptive(features)

# Read activily_labels:
activity_labels = read.table(
    paste(dataset_path, "activity_labels.txt", sep="/"),
    header=FALSE, col.names=c("num", "activity")
)

# Function, that reads full dataset of some type ("test" or "train")
read_data_set = function(type){
    
    path = str_c(dataset_path, "/", type)
    X = read.table(
        str_c(path, "/X_", type, ".txt"), header=FALSE, col.names = features_descriptive
    )
    Y = read.table(
        str_c(path, "/y_", type, ".txt"), header=FALSE
    )
    subject = read.table(
        str_c(path, "/subject_", type, ".txt"), header=FALSE
    )
    res = list()
    
    res = cbind("subject"=subject[,1], activity=activity_labels[Y[,1], "activity"], X)
    
    return (res)
}

print("Reading test data...")
test_data = read_data_set("test")

print("Reading train data...")
train_data = read_data_set("train")

print("Merging data into single table...")
merged_data = rbind(test_data, train_data)

print("Acquiring only data of mean and std measurements")
mean_and_std_columns = c(c(1,2), grep("mean", names(merged_data)))
mean_and_std_data = merged_data[, mean_and_std_columns]

print("Computing average value of each feature for subject and activity accordingly")
average_data = group_by(mean_and_std_data, activity, subject)
average_data = summarize_each(average_data, funs(mean))

print("Writing result tidy date to file")
write.table(average_data, "tidy.txt", row.names=FALSE)




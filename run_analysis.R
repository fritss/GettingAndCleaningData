library(data.table)

# the directory where all data is located
dataDir <- file.path(getwd(), "UCI HAR Dataset")

# the file with activity labels
actfile <- file.path(dataDir, "activity_labels.txt")

# the file with features
featfile <- file.path(dataDir, "features.txt")

# read the feature names
features <- read.table(featfile)
featureNames <- features$V2

# we are only interested in the means and the standard deviations
# these are feature names with mean(), meanFreq() and std() in it.
extractedFeatures <- grepl("mean\\(\\)|meanFreq\\(\\)|std\\(\\)", featureNames)

# read the activity labels
activity_labels <- read.table(actfile)

# the train data and test data must be handled in the same way
# to be sure that this is satisfied it is captured in a function
# that can read them both.
ReadDataFrame <- function(mode="train") {
  # the directory where the train/test data is located
  dir <- file.path(dataDir, mode)
  
  # the names of the files
  subfile <- file.path(dir, paste0("subject_", mode, ".txt"))
  xfile <- file.path(dir, paste0("X_", mode, ".txt"))
  yfile <- file.path(dir, paste0("Y_", mode, ".txt"))
  
  # read the three files
  x <- read.table(xfile)
  y <- read.table(yfile)
  subjects <- read.table(subfile)
  
  # give the features of the x-file descriptive names
  names(x) <- featureNames
  # and select only those columns we are interested in.
  x <- x[,extractedFeatures]
  # add a column indicating whether it is train or test data
  x$train_test_status <- mode
  
  # give the activities descriptive labels instead of numbers
  y[,1] <- activity_labels[y[,1], 2]
  
  # give the columns descriptive names
  names(y) <- "Activity_Label"
  names(subjects) <- "Subject"
  
  # combine the subjects, y and x in a single data frame
  cbind(subjects, y, x)
}

# read the training data
traindf <- ReadDataFrame("train")

# read the test data
testdf <- ReadDataFrame("test")

# combine them in a single frame
df <- rbind(traindf, testdf)

# calculate the average for every column grouped for every person and activity
# of course, we use dplyr and tidyr
library(dplyr)
library(tidyr)

# to avoid taking the average of the "train_test_status", this is part of the split
tidydf <-
  df %>%
  group_by (Subject, Activity_Label, train_test_status) %>%
  summarise_each(funs(mean))

# write out the results
datafile <- file.path(dataDir, "data.txt")
tidydatafile <- file.path(dataDir, "tidy_data.txt")

# the whole table is not required, only the tidy table
# write.table(df, file = datafile)
write.table(tidydf, file = tidydatafile, row.name=FALSE)

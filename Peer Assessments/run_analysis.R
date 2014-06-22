library(reshape2)
library(plyr)
## will be used to compare and load files;
datasetFilePart <- c("test", "train");
datasetFilePrefix <- c("subject", "X", "y");

## This method will check the availablity of the files in the default directory or directory provided.
## Default Directory is "./UCIHARDataset/"
checkDataFiles <- function(path="./UCIHARDataset/") {
  files <- dir(path);
  result <- FALSE;
  breakFlag <- FALSE;
  
  if(any(files == datasetFilePart[1]) && any(files == datasetFilePart[2])) {
    for(filePart in datasetFilePart) {
      files <- dir(paste(path, filePart, sep="/"));
      print(files);
      for(filePrefix in datasetFilePrefix) {
        file <- paste(filePrefix, "_", filePart, ".txt", sep="");
        print(file);
        if(any(files == file)) {
          result <- TRUE;
        } else {
          breakFlag <- TRUE;
          break;
        }
      }
      if(breakFlag) {
        result <- FALSE;
        break;
      }      
    }
  }
  
  result;
}

## this method will prepare one dataset as per provided datasetName ("train" or "test")
prepareDataset <- function(datasetName, path) {
  subjectDataset <- read.table(paste(path,"/", datasetName, "/", datasetFilePrefix[1], "_", datasetName, ".txt", sep=""));
  xDataset <- read.table(paste(path,"/", datasetName, "/", datasetFilePrefix[2], "_", datasetName, ".txt", sep=""));
  yDataset <- read.table(paste(path,"/", datasetName, "/", datasetFilePrefix[3], "_", datasetName, ".txt", sep=""));
  
  ## getting only activity Labels instead of activity ids.
  yDataset <- subset(getActivitiesWithLabel(path, yDataset), select=V2);
  
  ## setting up column names
  names(subjectDataset) <- c("subjectId");
  names(yDataset) <- c("activity");
  names(xDataset) <- getFeatures(path);
  
  xyDataset <- cbind(yDataset, xDataset);
  allMergeDataset <- cbind(subjectDataset, xyDataset);
  allMergeDataset;
}

## this method will return the data in features.txt file as a vector and will be to name the column of x dataset
getFeatures <- function(path){
  dfFeatures = read.table(paste(path, "/features.txt", sep=""));
  dfFeatures$V2;
}

## this method will merge the data provided in y files with activety labels file.
getActivitiesWithLabel <- function(path, dfActivities) {
  activityLabels <- read.table(paste(path, "/activity_labels.txt", sep=""));
  dfActivitiesWithLabels <- merge(dfActivities, activityLabels, all=TRUE);
  
  dfActivitiesWithLabels;
}


## Execution of the assignment
## if anyone want to read data from different files please change the path variable below.
path <- "./UCIHARDataset";

if(checkDataFiles(path)) {
  testDataset <- prepareDataset("test", path);
  trainDataset <- prepareDataset("train", path);
  trainAndTestDataset <- rbind(testDataset, trainDataset);
  
  ## creating first tidy dataset
  tidyDataset <- trainAndTestDataset[,grep("mean\\(\\)|std\\(\\)|subjectId|activity" , names(trainAndTestDataset), value=TRUE)];
  names(tidyDataset) <- gsub("mean", "Mean", names(tidyDataset)) # changing mean to Mean
  names(tidyDataset) <- gsub("std", "STD", names(tidyDataset)) # changing std to STD
  write.table(tidyDataset, file="tidydata.txt", sep = "\t", append=F);
  
  ## Q.5 tiddy Dataset with the average of each variable for every subject and activity.
  meltMergeDataset <- melt(tidyDataset, id=c("subjectId","activity"));
  tidyDataset2 <- dcast(meltMergeDataset, subjectId+activity ~ variable, mean);
  colnamesToRename <- colnames(tidyDataset2[3:68]);
  renamedColnames <- sapply(colnamesToRename, function(x) paste("each", "subject","activity", "average",x, sep="-"));
  colnames(tidyDataset2) <- c(colnames(tidyDataset[1:2]), as.vector(renamedColnames));
  write.table(tidyDataset2, file="tidydata2.txt", sep = "\t", append=F);
} else {
  stop("Please change the path value in the script file or provide data file in ./UCIHARDataset");
}
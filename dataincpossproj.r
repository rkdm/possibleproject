setwd("C:/Users/Rachel/Documents/jobdocs/specific applications/data incubator app/possibleproject")

### download the data
# download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
#               destfile = "activitydataset.zip")
# unzip("activitydataset.zip")

## tasks for this assignment:
##  --merge training & test sets together
##  --extract only the mean & stddev for each measurement
##  --use descriptive activity names to name the activities in the data set
##  --label the data set with descriptive variable names
##  --create a second, independent data set with the average of each variable for each activity and each subject
##      --use write.table() with row.name=FALSE
##  --include README.md and CodeBook.md also in repo

datadir <- "UCI HAR Dataset/"
dirs <- c("test","train")
activitylabels <- read.table(paste(datadir,
                    "activity_labels.txt",sep=""),
                    col.names=c("actcode","actname"))

for (d in dirs) {
    dataloc <- paste(datadir,d,"/",sep="")
    datasetfile <- paste("X_",d,".txt",sep="")
    labelfile <- paste("y_",d,".txt",sep="")
    subjfile <- paste("subject_",d,".txt",sep="")
    dataset <- read.table(paste(dataloc,datasetfile,sep="")) ## 561 columns
    labels <- read.table(paste(dataloc,labelfile,sep=""))  ## 1 column
    subjects <- read.table(paste(dataloc,subjfile,sep=""))  # 1 column
    data <- cbind(dataset,labels,subjects)
    if (d == "test") {
        alldata <- data
    } else {
        alldata <- rbind(alldata,data)
    }
}

featurenames <- read.table(paste(datadir,"features.txt",sep=""),
                           row.names=1,stringsAsFactors=FALSE) 
    ## use features.txt to create column names for variables
simplefeatnames <- gsub("[[:punct:]]","",
            as.vector(unlist(featurenames)))
    ## simplify data structure, and remove all the excess punctuation marks
    ##      that don't play well with other things
columnnames <- append(simplefeatnames,c("activitynum","subjectnum"))
    ## create one vector, with feature names and the other two variables saved
names(alldata) <- columnnames
    ## set the column names of 'alldata' to be the 'columnnames' vector

colMeans(alldata)
byact <- split(alldata,alldata$activitynum)
# str(byact)
# summary(byact)
bysubj <- split(alldata,alldata$subjectnum)
mapply(FUN=colMeans,byact)
mapply(FUN=colMeans,bysubj)
mapply(byact[[1]],FUN=hist)
res <- hist(byact[[1]][[1]])
res
test <- split(alldata,list(alldata$activitynum,alldata$subjectnum))
    ## result = list of 180 lists; each sublist is one activity for one subject
    ##   each of the 180 lists has 563 values
mapply(test[[1]],FUN=hist)
names(test)
summary(test[['1.1']])
summary(test[['3.1']])

#######  figure out how to pull out distributions/histograms of each variable for each activity
#######     --look at total 'Mag' variables
#######     --poss. also compare diff. individuals or look at distr for one indiv.




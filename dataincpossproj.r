### download the data
# download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
#               destfile = "activitydataset.zip")
# unzip("activitydataset.zip")

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


doublesplit <- split(alldata,list(alldata$activitynum,alldata$subjectnum))
## result = list of 180 lists; each sublist is one activity for one subject
##   each of the 180 lists has 563 values

## names(doublesplit) are things like '1.1', 
#       '3.1', etc.; first digit is activity, second is subject
## to pull out lists with only one activity or only one subject:
names(doublesplit[grepl("^1",names(doublesplit))])
    ## get names that start with "1" [activity #1]
names(doublesplit[grepl("25$",names(doublesplit))])
    ## get names that end with "25" [subject #25]

testvars <- c("tBodyAccMag", "tGravityAccMag",
              "tBodyAccJerkMag", "tBodyGyroMag",
              "tBodyGyroJerkMag")
# example values for each type of measurement
# [1] "tBodyAccMagmean"     "tBodyAccMagstd"      "tBodyAccMagmad"      "tBodyAccMagmax"      "tBodyAccMagmin"     
# [6] "tBodyAccMagsma"      "tBodyAccMagenergy"   "tBodyAccMagiqr"      "tBodyAccMagentropy"  "tBodyAccMagarCoeff1"
# [11] "tBodyAccMagarCoeff2" "tBodyAccMagarCoeff3" "tBodyAccMagarCoeff4"

## for now, just set specific variables to examine
var1 <- "tBodyAccMagmean"
var2 <- "tBodyAccJerkMagmean"
var3 <- "tBodyGyroMagmean"
var4 <- "tBodyGyroJerkMagmean"

## choose one activity:  #2
thisact <- names(doublesplit[grepl("^2",names(doublesplit))])
## choose one subject:  #17
thissubj <- names(doublesplit[grepl("17$",names(doublesplit))])

currentpars <- par()
par(mfrow=c(2,3),oma=c(0,0,2.5,0))

############
## plot all activities for one subject
for (s in 1:length(thissubj)) {
    thisactivity <- doublesplit[[thissubj[s]]][[var1]]
    thisactlabel <- levels(activitylabels$actname)[s]
    hist(thisactivity,prob=TRUE,breaks=30,
         cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
         xlab=thisactlabel,main=var1)
    curve(dnorm(x,mean(thisactivity),sd(thisactivity)),add=TRUE,col="red")
}
mtext(text="Mean of the Total Magnitude of Body Acceleration for each Activity for Subject #17",
      side=3, outer=TRUE)
## plot saved as 'subj17activities.png'

par(mfrow=c(3,3),oma=c(0,0,2.5,0))
############
## plot all subjects for one activity
for (a in 1:9) {
    thissubject <- doublesplit[[thisact[a]]][[var3]]
    thissubjlabel <- paste("Subject #",a,sep="")
    hist(thissubject,prob=TRUE,breaks=30,
         cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
         xlab=thissubjlabel,main=var3)
    curve(dnorm(x,mean(thissubject),sd(thissubject)),add=TRUE,col="red")
}
mtext(text="Mean of the Total Magnitude of Angular Velocity for Subjects 1 - 9 for SITTING",
      side=3, outer=TRUE)
## plot saved as 'sittingsubj1thru9.png'
par(currentpars)

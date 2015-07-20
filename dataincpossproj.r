setwd("C:/Users/Rachel/Documents/jobdocs/specific applications/data incubator app/possibleproject")

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

colMeans(alldata)
byact <- split(alldata,alldata$activitynum)
# str(byact)
# summary(byact)
bysubj <- split(alldata,alldata$subjectnum)
# mapply(FUN=colMeans,byact)
# mapply(FUN=colMeans,bysubj)
# mapply(byact[[1]],FUN=hist)
# res <- hist(byact[[1]][[1]])
# res
doublesplit <- split(alldata,list(alldata$activitynum,alldata$subjectnum))
    ## result = list of 180 lists; each sublist is one activity for one subject
    ##   each of the 180 lists has 563 values
# mapply(doublesplit[[1]],FUN=hist)
# names(doublesplit)
# summary(doublesplit[['1.1']])
# summary(doublesplit[['3.1']])

## names(doublesplit) are things like '1.1', 
#       '3.1', etc.; first digit is activity, second is subject
## to pull out lists with only one activity or only one subject:
names(doublesplit[grepl("^1",names(doublesplit))])
    ## get names that start with "1" [activity #1]
names(doublesplit[grepl("25$",names(doublesplit))])
    ## get names that end with "25" [subject #25]

## find the vars which are total "Mag"s
magvars <- simplefeatnames[grepl("Mag",simplefeatnames)]
testvars <- c("tBodyAccMag", "tGravityAccMag",
                "tBodyAccJerkMag", "tBodyGyroMag",
                "tBodyGyroJerkMag")
fulltestvarnames <- simplefeatnames[grep(testvars[1],simplefeatnames)]
    ## pull out all names that include tBodyAccMag
## result:
# > fulltestvarnames
# [1] "tBodyAccMagmean"     "tBodyAccMagstd"      "tBodyAccMagmad"      "tBodyAccMagmax"      "tBodyAccMagmin"     
# [6] "tBodyAccMagsma"      "tBodyAccMagenergy"   "tBodyAccMagiqr"      "tBodyAccMagentropy"  "tBodyAccMagarCoeff1"
# [11] "tBodyAccMagarCoeff2" "tBodyAccMagarCoeff3" "tBodyAccMagarCoeff4"

## how to pull out one subarray for fulltestvarnames[1]
foo <- doublesplit[[1]][[fulltestvarnames[[1]]]]
hist(foo,prob=TRUE)
curve(dnorm(x,mean(foo),sd(foo)),add=TRUE,col='BLUE')


## note to self: these arrays are not raw data, they are arrays of means, stddevs, etc.
##  --can construct a fake distribution given these numbers, like a normal using
#           mean, std, max, min
dnorm(data,mean=mean,sd=sd) ## generate probability density function (PDF)
curve(dnorm(x,mean,sd),add=TRUE,col='BLUE')  ## plot a real curve for the normal ftn
    ## must include 'x' no matter what your data/variables are -- it's not a 
    ##      real variable, it's just the placeholder it uses to figure out what to draw


#######  figure out how to pull out distributions/histograms of each variable for each activity
#######     --look at total 'Mag' variables
#######     --poss. also compare diff. individuals or look at distr for one indiv.


##corr.R: calculates correlation between nitrates and sulfates for ids with complete cases above a given threshold
#
#Written to fulfill Part 3 of Assignment 1 in Coursera Data Science R Programming course
#Created 6.3.15 (K. Divis)

corr <- function(directory, threshold = 0){
    currentWD=getwd()
    source("complete.R")
    cases=complete(directory)
    caseDim=dim(cases)
    counter = 0
    countLength=0
    caseCount=0
    for (i in 1:caseDim[1]){
        if (i<10) {fileName=paste("00",toString(i),".csv",sep="")}
        else if (i <100) {fileName=paste("0",toString(i),".csv",sep="")}
        else {fileName=paste(toString(i),".csv",sep="")}
        fpath = file.path(currentWD,directory,fileName)
        tempData=na.omit(read.csv(fpath, header=TRUE))
        caseCount=caseCount+1
        cases[caseCount,3]=cor(tempData$sulfate,tempData$nitrate)
    }
    
    for (i in 1:caseDim[1]){
        if (cases[i,2]>threshold){
            countLength=countLength+1
        }
    }
    if (countLength==0){output=numeric()}
    else{
        completeCases=data.frame(numeric(countLength),numeric(countLength),numeric(countLength))
        for (i in 1:caseDim[1]){
            if (cases[i,2]>threshold){
                counter=counter+1
                completeCases[(counter),1]=cases[i,1]
                completeCases[(counter),2]=cases[i,2]
                completeCases[(counter),3]=cases[i,3]
            }
        }
        output=completeCases[,3]
    }
    output
}
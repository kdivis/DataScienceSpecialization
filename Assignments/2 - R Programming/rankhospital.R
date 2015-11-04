# rankhospital() takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome),
#   and the ranking of a hospital in that state for that outcome (num).It reads the outcome-of-care-measures.csv 
#   file and returns a character vector with the name of the hospital that has the ranking specified by the num argument.


#Written to partially fulfill Assignment 3 in Coursera Data Science R Programming course
#Created 6.17.15 (K. Divis)



rankhospital <- function(state, outcome, num = "best") {
    #Load data
    data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
    
    #Check if valid args
    state.abbrev=sort(c(state.abb,"DC","GU","PR","VI"))
    isState=is.element(state,state.abbrev)
    if (isState==FALSE){
        stop("invalid state")
    }
    isOutcome=is.element(outcome,c("heart attack","heart failure","pneumonia"))
    if (isOutcome==FALSE){
        stop("invalid outcome")
    }
    
    data.sub <- subset(data, data[7]==state)
    library(plyr)
    
    if (outcome=="heart attack"){
        data.sub2 <- subset(data.sub, !is.na(data.sub[,11]))
        data.order <- arrange(data.sub2,data.sub2[11],data.sub2[2])
    }
    else if (outcome=="heart failure") {
        data.sub2 <- subset(data.sub, !is.na(data.sub[,17]))
        data.order <- arrange(data.sub2,data.sub2[17],data.sub2[2])
    }
    else if (outcome=="pneumonia") {
        data.sub2 <- subset(data.sub, !is.na(data.sub[,23]))
        data.order <- arrange(data.sub2,data.sub2[23],data.sub2[2])
    }
    
    
    sizeData=dim(data.order)[1]
    
    if (num=="best"){
        output=as.character(data.order[1,2])
    }
    else if (num=="worst"){
        output=as.character(data.order[sizeData,2])
    }
    else if (sizeData<num){
        output=NA
    }
    else {
        output=as.character(data.order[num,2])
    }     
    output
}

#rankall() takes two arguments: an outcome name (outcome) and a hospital ranking (num). It reads 
#   the outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital 
#   in each state that has the ranking specified in num.

#Written to partially fulfill Assignment 3 in Coursera Data Science R Programming course
#Created 6.17.15 (K. Divis)


rankall <- function(outcome, num="best"){
    #Load data
    data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
    
    isOutcome=is.element(outcome,c("heart attack","heart failure","pneumonia"))
    if (isOutcome==FALSE){
        stop("invalid outcome")
    }
    
    if (num=="best") num=1
    result.frame = as.data.frame(matrix(ncol=2,nrow=50),row.names=2)
    names(result.frame)=c("hospital","state")
    counter=0;
    state.abbrev=sort(c(state.abb,"DC","GU","PR","VI"))
    
    for (i in state.abbrev) {
        counter=counter+1
        result.frame[counter,2]=i
        data.state <- subset(data, data[,7]==i)
        
        if (outcome=="heart attack"){
            data.sub <- subset(data.state, !is.na(data.state[,11]))
            ranks=order(data.sub[,11],data.sub[,2])
            if (num=="worst") result.frame[counter,1]=as.character(data.sub[ranks[length(ranks)],2])
            else if (num>length(ranks)) result.frame[counter,1]=NA
            else result.frame[counter,1]=as.character(data.sub[ranks[num],2])
        }
        else if (outcome=="heart failure") {
            data.sub <- subset(data.state, !is.na(data.state[,17]))
            ranks=order(data.sub[,17],data.sub[,2])
            if (num=="worst") result.frame[counter,1]=as.character(data.sub[ranks[length(ranks)],2])
            else if (num>length(ranks)) result.frame[counter,1]=NA
            else result.frame[counter,1]=as.character(data.sub[ranks[num],2])
            
        }
        else if (outcome=="pneumonia") {
            data.sub <- subset(data.state, !is.na(data.state[,23]))
            ranks=order(data.sub[,23],data.sub[,2])
            if (num=="worst") result.frame[counter,1]=as.character(data.sub[ranks[length(ranks)],2])
            else if (num>length(ranks)) result.frame[counter,1]=NA
            else result.frame[counter,1]=as.character(data.sub[ranks[num],2])
        }
    }
    
    library(plyr)
    result.frame.reordered <- arrange(result.frame,result.frame[2])
    row.names(result.frame.reordered)=result.frame.reordered[,2]
    result.frame.reordered
}



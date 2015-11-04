#best(2-character abbrev name of a state, outcome name) and returns hospital with lowest mortality rate. 
#       Outcome is heart attack, heart failure, or pneumonia. Exclude data from hospitals that do not have 
#       that outcome. If there is a tie, then output first alphabetical hospital. Check validity of args.
#       If invalid state passed, use stop to "invalid state" (same for outcome).

#Written to partially fulfill Assignment 3 in Coursera Data Science R Programming course
#Created 6.17.15 (K. Divis)


best <- function(state, outcome){
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
    
    #Set correct DV
    if (outcome=="heart attack"){DVcol=11}
    else if (outcome=="heart failure") {DVcol=17}
    else if (outcome=="pneumonia") {DVcol=23}
    
    data.sub <- subset(data, data[7]==state)
    DV=data.sub[DVcol]
    lowMort=which(DV==min(DV,na.rm=TRUE))
    
    measures=data.frame(data.sub[2],DV)
    hospNames=sort(as.character(measures[lowMort,1]))
    bestHospital=hospNames[1]
    bestHospital    
}

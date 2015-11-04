##complete.R: calculates complete cases for a given set of ids
#
#Written to fulfill Part 2 of Assignment 1 in Coursera Data Science R Programming course
#Created 6.3.15 (K. Divis)


complete <- function(directory, id = 1:332) {
    currentWD=getwd()                                                       #Set working directory based on "directory" input
    nobs=numeric(length(id))                                                #Calculate how many ids will be checked
    noNA=numeric(2)                                                         #Initiate noNA and a counter
    counter=0
    
    for (i in id){                                                          #This loop pulls data by ID and saves all into data frame
        if (i<10) {fileName=paste("00",toString(i),".csv",sep="")}          #Add 0s (if necessary) to id numbers so that match formatting of file names
        else if (i <100) {fileName=paste("0",toString(i),".csv",sep="")}
        else {fileName=paste(toString(i),".csv",sep="")}
        fpath = file.path(currentWD,directory,fileName)
        tempData=na.omit(read.csv(fpath, header=TRUE))                   #Read data in .csv files to data.frame, omitting NAs
        noNA=dim(tempData)                                                  #Calculates dimensions of data.frame after NAs have been removed (how many complete cases for that id)
        counter=counter+1
        nobs[counter]=noNA[1]                                               #Puts number of complete cases for that id into a vector, nobs, which will be used in the output
    }
    
    output=cbind.data.frame(id,nobs)                                        #Outputs id and number of complete cases for that id
    output
}


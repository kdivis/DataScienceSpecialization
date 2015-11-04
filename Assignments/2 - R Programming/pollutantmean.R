##pullutantmean.R: calculates mean sulfate or nitrate levels for a given id
#
#Written to fulfill Part 1 of Assignment 1 in Coursera Data Science R Programming course
#Created 6.3.15 (K. Divis)


pollutantmean <- function(directory, pollutant, id = 1:332) {
    currentWD=getwd()
    counter=0;                                                          
    for (i in id){                                                          #This loop pulls data by ID and saves all into one data frame
        if (i<10) {fileName=paste("00",toString(i),".csv",sep="")}          #Add 0s (if necessary) to id numbers so that match formatting of file names
        else if (i <100) {fileName=paste("0",toString(i),".csv",sep="")}
        else {fileName=paste(toString(i),".csv",sep="")}
        fpath = file.path(currentWD,directory,fileName)
        tempData=read.csv(fpath, header=TRUE)                               #Read current .csv file into temporary data frame
        
        if (counter==0) {
            counter=1
            data=tempData}
        else {data=rbind(data,tempData)}                                    #Append temporary data frame to main data frame (that will hold data from ALL inputted ids)
    }
    
  
    if (pollutant=="sulfate") {output=mean(data$sulfate, na.rm=TRUE)}       #Outputs the mean of either sulfate or nitrate values (depending on args input)
    else if (pollutant=="nitrate") {output=mean(data$nitrate, na.rm=TRUE)}
    output
}
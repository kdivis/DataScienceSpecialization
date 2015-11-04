#Answers questions outlined in Course Project 2 (see below)

#Written for Coursera Data Science Exploratory Data Analysis course
#Created 7.19.15 (K. Divis)


################################################################################################################################

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


##############
# Question 1 #
##############
# 1. Have total emissions from PM2.5 decreased in the US from 1999 to 2008? Using the base plotting system, make a plot showing 
#    the TOTAL PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008

library(dplyr)

NEI99 <- filter(NEI,year==1999)
NEI02 <- filter(NEI,year==2002)
NEI05 <- filter(NEI,year==2005)
NEI08 <- filter(NEI,year==2008)

totalsMil=c(sum(NEI99$Emissions),sum(NEI02$Emissions),sum(NEI05$Emissions),sum(NEI08$Emissions))/1000000

barplot(totalsMil,names.arg=list("1999","2002","2005","2008"),main="Total PM2.5 Emissions in the US by Year",
        xlab="Year",ylab="PM2.5 levels (in millions of tons)")


##############
# Question 2 #
##############
# 2. Have total emissions from PM2.5 decreased in Baltimore City, Maryland (fips=="24510") from 1999 to 2009?
#    Use the base plotting system to make a plot answering this question.

library(dplyr)

NEI99Balt <- filter(NEI,year==1999 & fips=="24510")
NEI02Balt <- filter(NEI,year==2002 & fips=="24510")
NEI05Balt <- filter(NEI,year==2005 & fips=="24510")
NEI08Balt <- filter(NEI,year==2008 & fips=="24510")

totalsBalt=c(sum(NEI99Balt$Emissions),sum(NEI02Balt$Emissions),sum(NEI05Balt$Emissions),sum(NEI08Balt$Emissions))

barplot(totalsBalt,names.arg=list("1999","2002","2005","2008"),main="Total PM2.5 Emissions in Baltimore by Year",
        xlab="Year",ylab="PM2.5 levels (in tons)")


##############
# Question 3 #
##############
# 3. Of the 4 types of sources indicated by the type variable, which of these 4 sources have seen decreases in
#    emissions from 1999-2008 for Baltimore City? Which ahve seen increases in emissions from 1999-2008? Use
#    the ggplot2 system to make a plot answering this question.

libary(dplyr)
library(ggplot2)

NEIQ3 <- rbind(NEI99Balt,NEI08Balt)
NEIQ3$year=as.factor(NEIQ3$year)

qplot(data=NEIQ3,year,weight=Emissions, facets=.~type, main="Total PM2.5 Emissions in Baltimore by Type and Year",ylab="PM2.5 levels (in tons)",xlab="Year")


##############
# Question 4 #
##############
# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

library(dplyr)

SCCcoal <- filter(SCC, EI.Sector=="Fuel Comb - Electric Generation - Coal" | 
                      EI.Sector=="Fuel Comb - Industrial Boilers, ICEs - Coal" |
                      EI.Sector=="Fuel Comb - Comm/Institutional - Coal")

NEIQ4 <- NEI[NEI$SCC %in% SCCcoal$SCC,] 

NEI99Coal <- filter(NEIQ4,year==1999)
NEI02Coal <- filter(NEIQ4,year==2002)
NEI05Coal <- filter(NEIQ4,year==2005)
NEI08Coal <- filter(NEIQ4,year==2008)

totalsCoalThous=c(sum(NEI99Coal$Emissions),sum(NEI02Coal$Emissions),sum(NEI05Coal$Emissions),sum(NEI08Coal$Emissions))/1000

barplot(totalsCoalThous,names.arg=list("1999","2002","2005","2008"),main="Total PM2.5 Coal Combustion Emissions by Year",
        xlab="Year",ylab="PM2.5 levels (in thousands of tons)")


##############
# Question 5 #
##############
# 5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

library(dplyr)

NEI$year=as.factor(NEI$year)
NEI$type=as.factor(NEI$type)

NEI99BaltRoad <- filter(NEI,year==1999 & fips=="24510" & type=="ON-ROAD")
NEI02BaltRoad <- filter(NEI,year==2002 & fips=="24510" & type=="ON-ROAD")
NEI05BaltRoad <- filter(NEI,year==2005 & fips=="24510" & type=="ON-ROAD")
NEI08BaltRoad <- filter(NEI,year==2008 & fips=="24510" & type=="ON-ROAD")

totalsBaltRoad=c(sum(NEI99BaltRoad$Emissions),sum(NEI02BaltRoad$Emissions),sum(NEI05BaltRoad$Emissions),sum(NEI08BaltRoad$Emissions))

barplot(totalsBaltRoad,names.arg=list("1999","2002","2005","2008"),main="Total PM2.5 Emissions from Motor Vehicles in Baltimore by Year",
        xlab="Year",ylab="PM2.5 levels (in tons)")


##############
# Question 6 #
##############
# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in 
#    LA country (fips=="06037"). Whic city has seen greater changes over time in motor vehicle emissions
library(dplyr)
library(ggplot2)

NEI$year=as.factor(NEI$year)
NEI$type=as.factor(NEI$type)

NEIQ6 <- filter(NEI, year==1999 | year==2008)
NEIQ6 <- filter(NEIQ6, fips=="24510" | fips=="06037")
NEIQ6 <- filter(NEIQ6, type=="ON-ROAD")

NEIQ6b <- NEIQ6
for (i in 1:length(NEIQ6$fips)){
    if (NEIQ6$fips[i]=="24510") {NEIQ6b$fips[i]="Baltimore"}
    if (NEIQ6$fips[i]=="06037") {NEIQ6b$fips[i]="Los Angeles"}
}
NEIQ6b$fips <- as.factor(NEIQ6b$fips)


qplot(data=NEIQ6b,year,weight=Emissions, facets=.~fips, main="Total PM2.5 Emissions from Motor Vehicles in Baltimore and LA by Year",ylab="PM2.5 levels (in tons)",xlab="Year")


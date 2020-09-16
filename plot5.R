##Peer-graded Assignment: Course Project 2
## Developed by Andres Yrigoyen
##Fine particulate matter (PM2.5) is an ambient air pollutant for which there 
##is strong evidence that it is harmful to human health. In the United States, 
##the Environmental Protection Agency (EPA) is tasked with setting national 
##ambient air quality standards for fine PM and for tracking the emissions of
##this pollutant into the atmosphere. Approximatly every 3 years, the EPA 
##releases its database on emissions of PM2.5. This database is known as the
##National Emissions Inventory (NEI). You can read more information about the
##NEI at the EPA National Emissions Inventory web site.
##For each year and for each type of PM source, the NEI records how many 
##tons of PM2.5 were emitted from that source over the course of the entire
##year. The data that you will use for this assignment are for 1999, 2002, 2005,
##and 2008.
##Data
##The data for this assignment are available from the course web site as a 
##single zip file:
##Data for Peer Assessment [29Mb]
##The zip file contains two files:
##PM2.5 Emissions Data (\color{red}{\verb|summarySCC_PM25.rds|}summarySCC_PM25.rds)
## This file contains a data frame with all of the PM2.5 emissions data for 
##1999, 2002, 2005, and 2008. For each year, the table contains number of tons
##of PM2.5 emitted from a specific type of source for the entire year. 
##
##data example
##fips      SCC Pollutant Emissions  type year
## 4  09001 10100401  PM25-PRI    15.714 POINT 1999
## 8  09001 10100404  PM25-PRI   234.178 POINT 1999
## 12 09001 10100501  PM25-PRI     0.128 POINT 1999
## 16 09001 10200401  PM25-PRI     2.036 POINT 1999
## 20 09001 10200504  PM25-PRI     0.388 POINT 1999
## 24 09001 10200602  PM25-PRI     1.490 POINT 1999
##Where
##fips: A five-digit number (represented as a string) indicating the U.S. county
##SCC: The name of the source as indicated by a digit string (see source code classification table)
##Pollutant: A string indicating the pollutant
##Emissions|}Emissions: Amount of PM2.5 emitted, in tons
##type: The type of source (point, non-point, on-road, or non-road)
##year: The year of emissions recorded
##Source Classification Code Table (Source_Classification_Code.rd): This table
##provides a mapping from the SCC digit strings in the Emissions table to the
##actual name of the PM2.5 source. 
##Assignment
##The overall goal of this assignment is to explore the National Emissions 
##Inventory database and see what it say about fine particulate matter 
##pollution in the United states over the 10-year period 1999-2008. 
##You may use any R package you want to support your analysis.
##Step-1. Load data. The data was already downloaded in to my working directory
## Set working directory...Data analytic course/Case Study
##load the NEI and SCC data frames from the .rds files.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Now, Let see the data using head command
head(NEI)
head(SCC)
## Step-2. Load packages required for exploratory data analysis
library(plyr)
library(ggplot2)
## Converting "year", "type", "Pollutant", "SCC", "fips" to factor
Col_Factor <- c("year", "type", "Pollutant","SCC","fips")
NEI[,Col_Factor] <- lapply(NEI[,Col_Factor], factor)

head(levels(NEI$fips))
## The levels have NA as "   NA", so converting that level back to NA
levels(NEI$fips)[1] = NA
NEIdata<-NEI[complete.cases(NEI),]
colSums(is.na(NEIdata))

##Subset data for fips == "24510" 
NEIdataBaltimore<-subset(NEIdata, fips == "24510")
TotalEmissionsBaltimore <- aggregate(Emissions ~ year, NEIdataBaltimore, sum)
TotalEmissionsBaltimore
clean_names<-gsub("\\.","", clean_names)
SCCcombustion<-grepl(pattern = "comb", SCC$SCCLevelOne, ignore.case = TRUE)
SCCCoal<-grepl(pattern = "coal", SCC$SCCLevelFour, ignore.case = TRUE)

SCCCoalCombustionSCC<-SCC[SCCcombustion & SCCCoal,]$SCC
NIECoalCombustionValues<-NEIdata[NEIdata$SCC %in% SCCCoalCombustionSCC,]
NIECoalCombustionTotalEm<-aggregate(Emissions~year, NIECoalCombustionValues, sum)

SCCvehicle<-grepl(pattern = "vehicle", SCC$EISector, ignore.case = TRUE)
SCCvehicleSCC <- SCC[SCCvehicle,]$SCC
## using this boolean vector get the interested rows from the baltimore data
NEIvehicleSSC <- NEIdata[NEIdata$SCC %in% SCCvehicleSCC, ]
NEIvehicleBaltimore <- subset(NEIvehicleSSC, fips == "24510")
NIEvehicleBaltimoreTotEm<-aggregate(Emissions~year, NEIvehicleBaltimore, sum)

##Question-6 solution
##Compare emissions from motor vehicle sources in Baltimore City
##with emissions from motor vehicle sources in Los Angeles County, 
##California (fips == "06037"|}fips == "06037"). Which city has seen 
##greater changes over time in motor vehicle emissions?
NEIvehicleBaltimore<-subset(NEIvehicleSSC, fips == "24510")
NEIvehicleBaltimore$city <- "Baltimore City"
NEIvehiclela<-subset(NEIvehicleSSC, fips == "06037")
NEIvehiclela$city <- "Los Angeles County"
NEIBothCity <- rbind(NEIvehicleBaltimore, NEIvehiclela)
##Plotting the results
####Question-5 solution
##How have emissions from motor vehicle sources changed from 1999-2008 
##in Baltimore City?
##First of all,we need to subset the motor vehicles, we assume is anything 
##like Vehicle in EISector column
SCCvehicle<-grepl(pattern = "vehicle", SCC$EISector, ignore.case = TRUE)
SCCvehicleSCC <- SCC[SCCvehicle,]$SCC
## using this boolean vector get the interested rows from the baltimore data
NEIvehicleSSC <- NEIdata[NEIdata$SCC %in% SCCvehicleSCC, ]
NEIvehicleBaltimore <- subset(NEIvehicleSSC, fips == "24510")
NIEvehicleBaltimoreTotEm<-aggregate(Emissions~year, NEIvehicleBaltimore, sum)
##Plotting yearly Emissions
g<-ggplot(aes(year, Emissions/10^5), data=NIEvehicleBaltimoreTotEm)
g+geom_bar(stat="identity",fill="grey",width=0.75) +
    guides(fill=FALSE) +
    labs(x="year", y=expression("Total PM"[2.5]*" Emission in Baltimore (10^5 Tons)")) + 
    labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999 to 2008"))
dev.copy(png,"plot5.png", width=480, height=480)
dev.off()
##Plot 5 shows that the total emissions in Baltimore have been reduce substantially 
## from about 0.004 x10^5 in 1999 to less than 0.001 x10^5 in 2008.

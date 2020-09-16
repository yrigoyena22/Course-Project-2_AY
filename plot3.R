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
##Question 1 solution
##Have total emissions from PM2.5 decreased in the United States from 1999 
##to 2008? Using the base plotting system, make a plot showing the total 
##PM2.5 emission from all sources for each of the years 1999, 2002, 2005, 
##and 2008
TotalEmissions <- aggregate(Emissions ~ year, NEIdata, sum)
TotalEmissions
##Base plotting-Plotting total Emissions versus time using 
barplot(
    (TotalEmissions$Emissions)/10^6,
    names.arg=TotalEmissions$year,
    xlab="Year",
    ylab="PM2.5 Emissions (10^6 Tons)",
    main="Total PM2.5 Emissions From All US Sources"
)
## From the plot above, we can realize that the total PM2.5missions have 
##decreased in the US from 1999 to 2008
##The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999-2008. You may use any R package you want to support your analysis.
##Question-2 solution
##Have total emissions from PM2.5 decreased in the Baltimore City, 
##Maryland (\color{red}{\verb|fips == "24510"|}fips == "24510") from 1999
##to 2008? Use the base plotting system to make a plot answering this 
##question.
##Subset data for fips == "24510" 
NEIdataBaltimore<-subset(NEIdata, fips == "24510")
TotalEmissionsBaltimore <- aggregate(Emissions ~ year, NEIdataBaltimore, sum)

##Question-3 solution
##Of the four types of sources indicated by the type(point, nonpoint,
##onroad, nonroad) variable, which of these four sources have 
##seen decreases in emissions from 1999-2008 for Baltimore City? 
##Which have seen increases in emissions from 1999-2008? Use the 
##ggplot2 plotting system to make a plot answer this question.
g<-ggplot(aes(x = year, y = Emissions, fill=type), data=NEIdataBaltimore)
g+geom_bar(stat="identity")+
    facet_grid(.~type)+
    labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
    labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))+
    guides(fill=FALSE)

dev.copy(png,"plot3.png", width=480, height=480)
dev.off()
##we can see from the plot, the "NON-ROAD", "NONPOINT" and "ON-ROAD" type 
##of sources have shown a reduction in the total PM2.5 Emissions. 
##"POINT" type of source, shows the increase in the total PM2.5 emissions
##from 1999-2005. However, there is a  decrease in 2008. 
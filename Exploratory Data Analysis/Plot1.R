##The first part is to get the data and unzip it

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url,destfile="./Data.zip",method="curl")
unzip(zipfile="./Data.zip")

#read in Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 1.  Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total 
#PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(lattice)
library(ggplot2)
library(dplyr)
NEIdf <- tbl_df(NEI)

#pipe data to group by year and sum emissions by year and divid by 1 Million for scaling along y-axis
NEIsum <- NEIdf %>% select(Emissions, year) %>% group_by(year) %>%
        summarise(EmissionsSum=sum(Emissions)/1000000)

#plot data
xyplot(EmissionsSum ~ year,NEIsum, type = "l", 
       ylab = "Yearly Emissions (in Millions)", xlab="Year", 
       main="Total Emissions by Year")

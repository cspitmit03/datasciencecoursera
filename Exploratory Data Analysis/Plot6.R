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
SCCdf <- tbl_df(SCC)
#slice out vehicle items using filter command; 
#veh was used in grepl command instead of motor vehicle or only onroad types to capture the biggest set
veh <- filter(SCCdf, grepl("veh",Short.Name, ignore.case=T))

#find intersect and then subset those from NEI data frame
subset <- semi_join(SCC, veh, by ="Short.Name")
NEIveh <- semi_join(NEIdf, subset, by = "SCC")

NEIsum <- NEIveh %>% filter(fips %in% c("24510","06037")) %>% select(Emissions, year, fips) %>% 
        group_by(year, fips) %>% summarise(EmissionsSum=sum(Emissions))

xyplot(EmissionsSum ~ year | fips,NEIsum, type = "l", 
       ylab = "Yearly Emissions", xlab="Year", 
       main="Total Emissions by Year from Motor Vehicle Sources for \n Baltimore(24510) and Los Angeles (06037)")


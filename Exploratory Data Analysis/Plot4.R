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

#determine coal and combustion Short.Names from SCC
coal <- filter(SCCdf, grepl("coal",Short.Name, ignore.case=T))
comb <- filter(SCCdf, grepl("comb",Short.Name, ignore.case=T))
intersect <- intersect(coal, comb)

#subset NEI based upon prior criteria
subset <- semi_join(SCC, intersect, by ="Short.Name")
NEIcoalcomb <- semi_join(NEIdf, subset, by = "SCC")

#pipe data to determine sum of emissions by year
NEIsum <- NEIcoalcomb %>% select(Emissions, year) %>% 
        group_by(year) %>% summarise(EmissionsSum=sum(Emissions))

#diff between 1999 and 2008
NEIsum$EmissionsSum[1]-NEIsum$EmissionsSum[4]

#plot data
xyplot(EmissionsSum ~ year,NEIsum, type = "l", 
       ylab = "Yearly Emissions", xlab="Year", 
       main="US Total Emissions by Year for Coal / Combustion Sources")

